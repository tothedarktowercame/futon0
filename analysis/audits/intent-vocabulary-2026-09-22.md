# Intent vocabulary from recorded operator passages — 2026-09-22

Codex-14 reviewed the existing 100-pair pilot sample and selected 20 passage spans for explicit agent intent labels and rationales. These are stored inputs, not labels recomputed by phrase matching. They are not Joe-validated ground truth. The separate replay applies the revised Emacs phrase matcher to all 100 turns. All three sessions were consulted; this is a development replay, not held-out evaluation. The earlier six-way whole-turn stance labels are unchanged.

The revised vocabulary has 17 intent categories and 105 phrases. Standalone **but** is removed. No display strings or line breaks were restored. Existing !c human corrections and manually customized rules take precedence during installation; labels and rules remain distinct evidence.

| Session | Turns | Old matched | Old but-only | Revised matched |
|---|---:|---:|---:|---:|
| claude-12 | 43 | 25 | 16 | 34 |
| claude-4 | 43 | 19 | 13 | 27 |
| claude-5 | 14 | 12 | 10 | 10 |
| all | 100 | 56 | 39 | 71 |

The old matcher found a cue beyond but in only 17 turns. Revised coverage is 71/100, leaving 29 unmatched. These numbers measure matching, not correctness, recall of all intentions, or whole-turn classification. The new set deliberately loses some conjunction-only matches. Quoted speech, negation across phrase boundaries and unrecognized paraphrases can still mislead the matcher. A multi-intent passage is not forced into one category.

## Agent-labelled examples

| Intent | Passage | Reason |
|---|---|---|
| approve | Okay, I approve this plan of work. | Explicit approval of the proposed work. |
| delegate | So why don't you ask Claude 4 what my take on C is? | Redirects the information request to the agent holding the prior discussion. |
| delegate | you can bell claude-4 | Authorizes contacting another agent. |
| clarify | I don't understand the "needs owner" semantics | Requests clarification of a term, rather than rejecting the whole plan. |
| prioritize | So yes, please sort this out as a matter of priority. | Explicit priority for resolving the foundational definitions. |
| extend | And in parallel to this | Adds a concurrent testimony/comparison task to the work already approved. |
| clarify | can you please help me understand the situation with the A matrix? | Asks for an explanation of the implementation situation. |
| defer | OK, let's put it into the DAG and we'll do it when we get time. | Keeps the proposed work but postpones execution. |
| continue | So let's get on with it. | Requests execution after repeated next-step descriptions. |
| explain | And here's why. | Introduces the reasons for the objection in the following passage. |
| propose | Maybe we should create a system prompt for plop-2026 paper updates? | Suggests a specific intervention to improve paper revisions. |
| report-problem | I still see an HTTP error on https://zone.hyperreal.enterprises/marimo/ | Reports continuing observed failure. |
| disagree | that doesn't match my intent | Rejects the interpretation behind the locked-down demo. |
| clarify | How should we proceed? | Asks for a revised way forward after identifying the mismatch. |
| approve | The new diagram looks good | Approves the diagram itself. |
| delegate | we should ask codex-28 to update the shared diagram | Assigns the shared update to its owner. |
| collect | collect information from codex-7, codex-8, and codex-9 | Requests aggregation of worker information before updating. |
| constrain | Right now I am not asking you to dispatch work | Explicitly limits the current task to strategy rather than dispatch. |
| verify | we should check through /home/joe/code/mathlib4/DarkTower/WarMachine | Requires checking formal material before concluding a feature is missing. |
| redirect | I'd like to return to futon-2026 | Returns attention to the paper as a basis for business analysis. |

## Live vocabulary snapshot

- **approve**: I agree; I approve; that's a good fit; that's great; looks good; good news; sounds good; you're right; Yes you can do this; I definitely like the idea
- **disagree**: I disagree; I don't agree; I do not agree; that's wrong; misunderstood my intent; doesn't match my intent; does not match my intent; of no use whatsoever; a bad design
- **clarify**: I don't understand; help me understand; I'd like to know; I'd like to see some examples; what's the story; What gives; I'm slightly confused; How should we proceed; what's our strategy
- **propose**: I suggest; We could perhaps; maybe we could; Maybe we should; I wonder if; I think it would be good
- **extend**: in parallel; Another thing we should pay attention to; we should also; we could also; that's another analysis; could be a further set of tasks
- **prioritize**: as a matter of priority; please do that next step; needs to be the next; first instance; we have 7 minutes
- **delegate**: please bell; you can bell; let's ask; ask Zai; we should ask; should be sent to; for dispatches; you will be responsible for
- **verify**: we should check; we need to check; we need to be able to validate; we'll have to check; we have to audit; I want a reproduction
- **constrain**: don't do that; I am not asking you to; I don't want to spend; please use aliases; we will not do any deep dives; I don't want a repeat; not going to decide things by fiat
- **defer**: we'll do it when we get time; we can come back to; at some point; for now; defer processing
- **continue**: please continue; go on; get on with it; let's continue; Please do 1, 2, and 3
- **redirect**: rather than; let's trim; we will instead focus; I'd like to return to; what we should do is; I want to alter
- **explain**: here's why; my main point; what I mean; the broader long term idea; the use cases would be; my use case
- **report-problem**: is currently broken; I still see an HTTP error; it's broken; login doesn't work; overlaps existing UI elements; point of major concern
- **collect**: collect information; getting logs; keep a record; record the turns
- **qualify**: with the caveat; to the extent that it is possible
- **ask-action**: can you please; please publish; please sort this out; I would like to have; please update

## Provenance and verification

Runtime authority: `futon3c/emacs/session-mode.el`, `session-mode-turn-intent-vocabulary`; the replay JSON is a generated snapshot, not a second mutable vocabulary authority. Per-span source IDs and zero-based character offsets are in the annotations JSONL. Source hash is in replay JSON.

Commands: `emacs -Q --batch -L futon3c/emacs -l /tmp/measure-intents.el` replayed both vocabularies using the actual matcher; `emacs -Q --batch -L futon3c/emacs -l futon3c/test/session-mode-test.el -f ert-run-tests-batch-and-exit` checked boundaries, mixed intent and terminal layout preservation. Replay input loops over each JSONL operator_text and calls session-mode--turn-matches with the old 15-phrase vocabulary and the new default; output records every start/end/tag/matched-text tuple.

No Haiku service or background per-turn model has been installed. The agent doing this interpretation is Codex-14, in this task. Generalization is currently the inspectable phrase vocabulary distilled from this pass. Future agent passes can expand the labelled corpus from session logs and Joe’s !c corrections without doing inference on every keystroke.
