# Algorithm: Conduct Problem Intake With Route Questions

Date: 2026-10-11
Status: spec (run by hand from the first session; encode only what survives three)
Owner: Joe (drafted by claude-11)

## Purpose

Settle, before work starts, *what problem is being solved* and *by which route*,
and train the habit of asking that well. The algorithm produces an intake record
that separates the requester's goal from any method that arrived with the request,
names the authority that defines correct, asks whether that authority can simply
be run, and writes down what would show the chosen route wrong.

It has two modes:

- **Live intake**: run on a new problem before the first piece of work.
- **Drill**: run cold on a closed past problem, from a packet that holds only
  what was known on day one, then compare with what actually happened. The drill
  trains the questions; the live mode applies them.

The questions come from one observed failure shape (below) and from the question
the futon-2026 working paper asks of the whole stack, "What problems are we
solving?" (`p4ng/futon-2026.tex`; `futon0/holes/missions/M-futon-problems.md`).
WR-0 asks for an open model, an available exit and a verdict the apparatus cannot
issue to itself; at intake these are, respectively, the route as a hypothesis, the
route falsifier, and the requester's acceptance test.

Prerequisites:
- a requester (a person, or a mission with a named operator) who can be asked
  what result they would accept;
- for drill mode, a closed case with a known change of route, and someone other
  than the trainee to prepare its day-one packet.

Scope and non-ownership:
- does not file or triage issues: `create-futon-issue-before-triage.md`,
  `create-mfuton-issue-before-triage.md`,
  `triage-futon-issue-and-decide-local-work-handoff.md` own that;
- does not run postmortems, though postmortems are the main source of drill
  cases: `conduct-post-mortem-learning-capture-discussion.md`;
- does not choose the route for the requester; it records routes, their tests
  and their falsifiers, and the requester's acceptance test decides.

## The failure shape this was built from

A requester with a clear goal hands over a method they have been using: re-
implement an existing system in a new host by porting it piece by piece, with each
repair held to account. The team adopts the method, maps the existing system's
internals to decide what the port must reproduce, and wires the existing system up
as a comparator. Measurements of the existing system are made, but each is read as
"what must the port reproduce". For days progress is counted along the chosen
route. Then someone asks what the existing system already provides, and finds that
its own compiled form can be run in the new host; the goal is met within a day.

Stage by stage: a map of the reference, then a copy checked against the
reference, then the reference itself (Joe: "a precession of simulacra for the real
solution"). Neither the method nor the map was wrong; both were the slow way to the
same goal. The questions below are those that, asked on day one, would have found
the short route. (The case is private; this algorithm keeps only its shape.)

## Companion Flexiarg Resolution (2026-10-11)

Governing companion flexiargs (futon3 `library/parsing/`, drafts):
- `parsing/adopt-the-goal-not-the-method`: Q1, Q2 and Q6
- `parsing/run-the-reference-before-modelling-it`: Q3 and Q4
- `parsing/a-comparator-points-at-its-own-rehosting`: Q5
- `parsing/check-output-with-the-real-parser`: Q7
- `parsing/audit-the-route-not-only-the-repairs`: Q8 and the review step

Inspected but non-owning:
- `coordination/intent-to-mission-binding`: binds tasks to a mission reference;
  this algorithm supplies what the binding should point at (the goal and its
  acceptance test, not a method the mission text happens to state)
- `war-room/wr-0-organise-without-apparatus`: source of the verdict/exit framing

Execution-time rule: when this algorithm runs, read the governing flexiargs above;
they hold the reasons, this file holds the order.

## Inputs

- the request, verbatim, and any method, code, plan or mission that came with it
- what already exists: the systems, binaries, data, prior attempts in reach
- drill mode: a day-one packet (see Drill) and a timer

## The intake questions

Answer each in writing. "Unknown, ask X" is an answer; a blank is not.

- **Q1 Goal.** What observable result would the requester accept as the goal met?
  Write it as a check someone could run. If it cannot be written, ask the
  requester before going on.
- **Q2 Method.** What method arrived with the request? Write it as one candidate
  route, separate from Q1. Note what in it is a constraint the requester actually
  holds, and what is only how they had been working.
- **Q3 Authority.** What defines correct: a reference system, a specification, a
  dataset, a person's judgement? Name the exact version.
- **Q4 Executable.** Does a runnable form of that authority exist (binary,
  bytecode, intermediate representation, service)? Could it run in the target
  setting, wholly or in part? If plausibly yes, add "run the reference" as a
  candidate route, and name the cheapest test of it.
- **Q5 Comparator.** If results will be compared with the authority, what code
  does the comparator run? Could that code be the implementation, with the
  comparator kept to check the hosting?
- **Q6 Two readings.** For each measurement of the target planned at intake, write
  both questions it will answer: what does our method need, and what does the
  target already provide?
- **Q7 Consumer.** What will read the output (a parser, a person, another
  program)? Plan to check output with that reader, not with a model of it.
- **Q8 Route falsifier.** For the chosen route: what result, or what pace by what
  date, would show another candidate is better? When will this be looked at?
- **Q9 Problem.** In one sentence: whose problem is this, and what changes for them
  when Q1 passes? If this cannot be said, the intake is not finished.

## Procedure: live intake

1. Copy the request and anything handed over into the intake record (below),
   verbatim, before interpreting it.
2. Answer Q1 and Q2 first, and keep them apart. Confirm Q1 with the requester
   when it was inferred.
3. Answer Q3 to Q5. Spend the first discovery effort (hours, not days) on the
   cheapest test named in Q4, before any mapping or porting.
4. List the candidate routes (at least the requester's method and, where Q4 found
   one, running the reference), each with its cheapest decisive test and a guessed
   cost. Write the guesses before running the tests.
5. Run the cheapest decisive tests; choose a route; answer Q7 and Q8 for it.
6. Answer Q9. Write the record where the work's notes live, not under `/tmp`.
7. At the Q8 date, and whenever pace stalls, reopen the record: re-run Q4 to Q6
   against what has been learned, and ask whether the stall is structural.

## Procedure: drill

Training the questions on a closed case whose outcome is known.

1. **Prepare the packet** (not the trainee). From a closed case with a known
   change of route, write a day-one packet: the request as received, the method
   handed over, what existed and was in reach, and nothing learned afterwards. For
   private cases, write it at the level of shape, as in the failure shape above.
2. **Cold attempt** (trainee, 20 minutes, no assistance). Answer Q1 to Q9 from the
   packet. Rank the candidate routes, and for each give its cheapest test and a
   guessed cost.
3. **Self-diagnosis** (5 minutes, before the reveal). Which question was hardest
   to answer, and where did the answer lean on the handed-over method?
4. **Reveal and compare** (10 minutes). Read what happened. Score:
   - *hit*: one of the ranked routes matches the route that worked; record its rank;
   - *finding question*: the question whose answer would have surfaced that route;
   - *captured answers*: answers given in the inherited method's terms (Q6's first
     reading only, or Q4 answered "not our method").
5. **Log** the drill in the ledger: case id, date, trainee, hit and rank, finding
   question, captured answers, minutes over time.

Invariant: the packet carries no hindsight. A packet that hints at the answer
measures recall of the case, not the habit. Whoever prepares a packet does not
take that drill.

## Drill cases

Seed: the failure shape above, as packet "inherited port with a runnable reference".
Further cases are to be harvested, not invented:
- postmortems that record a change of route;
- missions with a redirect note from the operator;
- the problem panel of `futon0/holes/missions/M-futon-problems.md`.

A case enters the drill set once its day-one packet has been written and checked
by someone who knows the case.

## Decision Rule

1. Q1 is answered before any route is chosen; an unconfirmed inferred goal is
   recorded as such.
2. The requester's method is always one candidate route, never the frame for the
   others.
3. Where Q4 finds a plausible runnable reference, its cheapest test runs before
   any model, map or port is started.
4. A route without a Q8 falsifier and review date is not chosen yet.
5. Drill results change this algorithm only when a pattern survives three
   sessions.

## Outputs

- **Intake record** (live mode), one file per problem, fields: request (verbatim),
  Q1 to Q9, candidate routes with tests, guessed and actual costs, chosen route,
  review date, and later review entries.
- **Drill ledger entry** (drill mode), fields as in drill step 5.

## Stop Conditions

- Q1 cannot be written and the requester is unavailable: stop and ask; do not
  substitute the method for the goal.
- The cheapest Q4 test needs access or authority the intake does not have: record
  it as a blocked route and route the request to whoever has the access.
- In drill mode, the packet turns out to contain hindsight: discard the attempt,
  repair the packet, and use a different trainee.

## Case Anchors

- the failure shape above (a private port, 2026-09/10), and its postmortem,
  recorded as futon3 `library/parsing/`
- `python-dojo.md`: the drill protocol (cold attempt, self-diagnosis before the
  reveal, compare) is adapted from it

## References

- `p4ng/futon-2026.tex` ("What Problems Are We Solving?")
- `futon0/holes/missions/M-futon-problems.md`
- futon3 `library/parsing/README.md` and its five patterns
- futon3 `library/translation/README.md`
- futon3 `library/war-room/wr-0-organise-without-apparatus.flexiarg`
- `conduct-post-mortem-learning-capture-discussion.md`
- `python-dojo.md`
