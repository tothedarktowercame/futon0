You are one of 23 agents, each assigned one node ("R-node") of the War Machine's active-inference control loop. Your node is given at the end. You work alone, read-only: do not edit, create or commit any file. Your whole output is your final message, which the runner saves as {NODE}-links.edn.

The aim: align library patterns with R-nodes, so that operator turns (which retrieve patterns) can be read as observations at R-nodes in an AIF model of the operator. Each R-node agent bids for the patterns its code actually carries out. A bid is a Nelson-style link from a code declaration to a pattern, so the bids also annotate the code with patterns.

ROUND 1 — PRIME. Read your node's code and records until you can say what the node computes, citing declarations.
- Your node's row in /home/joe/code/p4ng/empirics-futon/control-stages.edn (stage, band, label, :basis if any).
- Your node's Lean files (listed below; may be none).
- The equations registry /home/joe/code/futon2/holes/labs/wm-contract/aif-equations.edn: the equations and pointers for your node.
- Runtime code: find where your node is computed in the War Machine runtime (start from /home/joe/code/futon2/scripts/futon2/report/war_machine.clj and /home/joe/code/futon2/src/futon2/aif/; `rg -n 'R14\b'`-style searches for your node id help). Also /home/joe/code/futon2/holes/labs/wm-contract/ALIGN-rnode-process-census.md and facts-<NODE>.md if one exists. Numbering: the catalogue numbering in control-stages.edn, which differs from older contract numbering (/home/joe/code/p4ng/R-concordance.md).
Write 3-8 priming claims, each citing a declaration at file:line that you have opened.

ROUND 2 — BID. The candidate patterns are in /home/joe/code/futon0/analysis/audits/rnode-bids/patterns.md (150 rows: id, stage label, hits, title, conclusion, THEN, path). Bid for patterns whose THEN your node's code actually performs or enforces.
- Open the pattern's file before bidding on it.
- Every bid cites one code declaration (Lean or Clojure) that you have opened, at file:line, and says in ONE sentence how that code does what the pattern's THEN says.
- At most 15 bids. Zero bids is a valid answer. You may bid on a pattern labelled with any stage; a cross-stage bid is informative, not an error.
- :kind :functional when the code carries out the pattern's practice; :kind :topic when the pattern is only ABOUT your node (e.g. problems/r14-...) without the code performing its THEN. Prefer functional bids; list at most 3 topic bids.
- :strength :strong (the code does the THEN) or :partial (it does part of it; say which part in :how).
- Do not bid by keyword match. If the only connection is a shared word, decline.
- Record up to 5 patterns you considered and declined, with the reason.

OUTPUT: your final message must be exactly one EDN map and nothing else (no prose, no code fence):
{:node "<NODE>"
 :priming [{:claim "..." :symbol "..." :file "/abs/path" :line 123} ...]
 :bids [{:pattern "family/name" :kind :functional :strength :strong
         :code {:lang :lean :symbol "Ns.decl" :file "/abs/path" :line 123}
         :how "one sentence"} ...]
 :declined [{:pattern "family/name" :why "..."} ...]}
Paths absolute; :line is the line where the declaration starts; :lang is :lean or :clojure (or :emacs-lisp / :python if that is truly where it lives).

YOUR NODE:
