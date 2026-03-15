;;; drawbridge-eval-test.el --- Tests for drawbridge-eval -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'drawbridge-eval)

(ert-deftest drawbridge-eval-uses-explicit-token-first ()
  (let ((drawbridge-eval-token "abc123"))
    (should (equal "abc123" (drawbridge-eval--token)))))

(ert-deftest drawbridge-eval-returns-value-on-success ()
  (cl-letf (((symbol-function 'drawbridge-eval-request)
             (lambda (_clj-code &optional _timeout)
               '((ok . t) (value . "done")))))
    (should (equal "done" (drawbridge-eval "(+ 1 2)")))))

(ert-deftest drawbridge-eval-parses-edn-like-success-response ()
  (should
   (equal '((ok . t) (value . 3) (error . nil) (type . nil))
          (drawbridge-eval--parse-response-body "{:ok true, :value 3}"))))

(ert-deftest drawbridge-eval-parses-edn-like-error-response ()
  (should
   (equal '((ok . nil)
            (value . nil)
            (error . "Syntax error compiling at (1:1).")
            (type . "clojure.lang.Compiler$CompilerException"))
          (drawbridge-eval--parse-response-body
           "{:ok false, :error \"Syntax error compiling at (1:1).\", :type \"clojure.lang.Compiler$CompilerException\"}"))))

(ert-deftest drawbridge-eval-signals-on-error-response ()
  (cl-letf (((symbol-function 'drawbridge-eval-request)
             (lambda (_clj-code &optional _timeout)
               '((ok . nil) (error . "boom")))))
    (should-error (drawbridge-eval "(throw (ex-info \"x\" {}))"))))

;;; drawbridge-eval-test.el ends here
