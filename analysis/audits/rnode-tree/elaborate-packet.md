Task: elaborate a classification tree that reads operator turns down to R-nodes. You work independently; two other agents get the same task, and their answers are compared with yours afterwards. Do not look at their files.

Background: we are building an active-inference model of the operator (Joe). The R-nodes are the War Machine's catalogue of requirements that any viable controller has to meet: /home/joe/code/p4ng/empirics-futon/control-stages.edn. They are existential requirements, not conversational acts. The draft tree is /home/joe/code/futon0/analysis/audits/rnode-tree/rnode-tree.edn. Read its header comment. The splits are:
1. Beer's Viable System Model system.
2. Max-Neef need × existential mode (Human Scale Development, 1991).
3. Inside System 4's Understanding cells only: whether the turn is about the world (evidential marking: "I see", "it returned") or about the account of it (epistemic modality: "I think", "I was wrong").
Each node carries :cues, keywords and phrasings Joe might write.

Data: /home/joe/code/futon0/analysis/audits/rnode-tree/dev-turns.json holds 150 of Joe's turns, with opaque ids. Each has the tail of the agent message it answered. Do not look up their sources. Treat any text Joe pasted from an agent (tables, quoted agent paragraphs) as not his words.

Do all four:
1. CUES. For any branch or leaf, propose additional cues: words or short phrasings, in Joe's register, that would identify that node. Every cue must cite a dev turn that prompted it, with the turn id and Joe's exact words containing it, or be marked :from :general if it comes from your own understanding of the node. Aim for 3-8 per leaf where the turns support it. Do not pad.
2. COLLISIONS. List cues, existing or proposed, that would fit two or more leaves, and say which split fails to separate them.
3. STRUCTURE. You may NOT move leaves or change splits. You may flag a placement you think is wrong, with a reason and the alternative.
4. MAX-NEEF CHECK. The Max-Neef cell terms in the file were written from memory. For each cell the tree uses, say whether the need × mode pairing and the italicised terms match Max-Neef's published matrix as you know it, and give the terms you believe the matrix has for that cell. Say how sure you are. Do not invent citations.

Output: write exactly one file, /home/joe/code/futon0/analysis/audits/rnode-tree/elab-{AGENT}.edn, and nothing else. Do not edit the tree, and do not commit. Shape:
{:agent "{AGENT}"
 :cues [{:node "R14" :cue "lean towards" :turn "T027" :quote "exact words from that turn"} {:node :system-3 :cue "..." :from :general} ...]
 :collisions [{:cue "..." :nodes ["R2" "R4"] :why "..."} ...]
 :placement-flags [{:node "R13" :now "system-4 > protection·doing" :suggest "..." :why "..."} ...]
 :max-neef [{:cell [:creation :doing] :pairing-ok true :terms ["work" "invent" ...] :confidence :high|:medium|:low :note "..."} ...]}
(:node is a leaf id string, or a branch keyword such as :system-3.)
Check that it parses as EDN, e.g. python3 -c "import edn_format,sys;edn_format.loads(open(sys.argv[1]).read())" FILE.
