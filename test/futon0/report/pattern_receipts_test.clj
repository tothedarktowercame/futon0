(ns futon0.report.pattern-receipts-test
  "Tests the footer parser (pure) and the invariant against a real throwaway
  git repository — the exact bad cases the invariant is named for: a claimed
  commit with no footer, a footer whose pattern-ref does not resolve, and the
  honest pass/vacuous distinction."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [futon0.report.pattern-receipts :as pr])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:dynamic *repo* nil)

(defn- sh [dir & args]
  (let [{:keys [exit out err]}
        (apply clojure.java.shell/sh
               (concat ["git" "-C" (str dir)] args))]
    (assert (zero? exit) (str args " failed: " err out))
    (str/trimr out)))

(defn- commit!
  [repo msg & paths]
  (doseq [p paths]
    (let [f (io/file repo p)]
      (.mkdirs (.getParentFile f))
      ;; vary content so git log -- <pathspec> never drops the commit
      (spit f (str "stub " (rand)))))
  (sh repo "add" "-A")
  (sh repo "commit" "--allow-empty" "-m" msg))

(defn fixture [f]
  (let [dir (.toFile
             (Files/createTempDirectory "pattern-receipts-"
                                        (make-array FileAttribute 0)))]
    (sh dir "init" "-q")
    (sh dir "config" "user.email" "test@example.com")
    (sh dir "config" "user.name" "test")
    (commit! dir "base: unrelated" "README.md")
    ;; pre-cutover commit in claimed region, no footer — out of scope
    (commit! dir "pre-cutover library edit" "library/futon-theory/base.flexiarg")
    ;; good: footer, ref resolves (file exists from the pre-cutover commit)
    (commit! dir (str "ok edit\n\nPattern: futon-theory/base")
             "library/futon-theory/base.flexiarg")
    ;; bad 1: claimed commit, no footer
    (commit! dir "silent library edit" "library/pattern-discipline/silent.flexiarg")
    ;; bad 2: footer whose ref does not resolve
    (commit! dir (str "edit with ghost footer\n\nPattern: pattern-discipline/ghost")
             "library/futon-theory/base.flexiarg")
    (binding [*repo* (str dir)]
      (f))))

(use-fixtures :each fixture)

(deftest footer-parser
  (is (= ["futon-theory/base"] (pr/parse-pattern-footers "edit\n\nPattern: futon-theory/base")))
  (is (= [] (pr/parse-pattern-footers "we discussed Pattern: futon-theory/base in prose.")))
  (is (= [] (pr/parse-pattern-footers "Pattern: Not A Pattern")))
  (is (= ["a/b"] (pr/parse-pattern-footers "Pattern: a/b\nPattern: a/b"))
      "last occurrence wins: repeats collapse"))

(deftest claimed-region
  (is (pr/pattern-claimed-region? "library/pattern-discipline/x.flexiarg"))
  (is (not (pr/pattern-claimed-region? "src/futon/x.clj")))
  (is (not (pr/pattern-claimed-region? "library/README.md"))))

(deftest invariant-detects-the-named-bad-cases
  (let [result (pr/invariant-pattern-receipts-resolve *repo* "--all")]
    (is (= :fail (:status result)) (pr-str result))
    (is (= 4 (:commits-checked result)))
    (is (= #{:missing-pattern-footer :unresolvable-pattern-ref}
           (set (map :kind (:violations result))))
        "both named bad cases are caught")))

(deftest invariant-passes-when-scoped-after-the-bad-commits
  (let [head (sh *repo* "rev-parse" "HEAD")
        result (pr/invariant-pattern-receipts-resolve *repo* head)]
    (is (= :pass (:status result)))
    (is (:vacuous result) "empty post-cutover set is honestly vacuous")))

(deftest invariant-fails-at-cutover-boundary
  ;; scope from the root commit: the two bad commits are in range
  (let [root (sh *repo* "rev-list" "--max-parents=0" "HEAD")
        result (pr/invariant-pattern-receipts-resolve *repo* root)]
    (is (= :fail (:status result)))))
