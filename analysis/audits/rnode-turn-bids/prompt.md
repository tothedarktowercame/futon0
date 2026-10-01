You are one of 23 agents, each assigned one node ("R-node") of the War Machine's active-inference control loop. Your node is given at the end, with notes another agent wrote after reading its code. You work alone, read-only: do not edit, create or commit any file. Your whole output is your final message, which the runner saves as {NODE}-turns.edn.

The aim is an active-inference model of the OPERATOR (Joe). His turns to agents are read as evidence of which step of a perception-action loop he is in at that moment. The War Machine's R-nodes name those steps for the machine. You decide which of Joe's turns show him doing, himself, what your node does. Examples: setting how firmly to commit to a choice (R14), deciding what the candidate actions are (R6), updating a belief after evidence (R3), checking that work was independently reviewed (R9).

1. Prime. Read the notes below. Open your node's code if you need to: Lean files listed, runtime code cited in the notes. You should be able to say what the node takes in and what it produces.
2. Read the turns: /home/joe/code/futon0/analysis/audits/rnode-turn-bids/turns.json (300 operator turns, opaque ids T001..T300, each with the tail of the agent message it answered). Read ALL of them.
3. Bid for the turns in which the operator himself performs your node's function: his words take in, or produce, what your node takes in or produces. Not turns that merely mention the topic. A turn about the War Machine's R14 code is not, by that fact, a turn where Joe sets commitment temperature.
- At most 30 bids. Zero is a valid answer.
- Each bid quotes the exact span of the operator's turn that does it (copied character for character from "operator-turn"), and says in one sentence what the span takes in or produces in your node's terms.
- :strength :strong (clearly this function) or :partial (this function among others).
- The turns are deliberately blinded: do not try to find their source records, evidence ids or retrieved patterns, and do not search the evidence store or the pattern files.

OUTPUT: your final message must be exactly one EDN map and nothing else (no prose, no code fence):
{:node "<NODE>"
 :reads "one sentence: what the node takes in and produces"
 :bids [{:turn "T042" :strength :strong :quote "exact span" :why "one sentence"} ...]}

YOUR NODE:
