# Intake: Alinery and Mastra deep dives (M-landscape-positioning §6)

Run of `~/code/algorithms/conduct-problem-intake-with-route-questions.md`
(live mode), 2026-10-11, claude-11 with Joe. Written before either dive starts, so
that the dives can be checked against it.

The algorithm's failure shape applies in both directions here:
- **their method leads**: their docs explain how they work, and reading them
  first makes "what problem are they solving" come out in their own terms;
- **our method leads**: §6 already pairs each neighbour with a FUTON counterpart
  (Alinery to pattern cascades, Mastra to War Machine R-nodes). Reading them in
  order to fill that table is how the earlier port read Lean in order to decide
  what the port must reproduce.

Two levels of intake follow: Joe's request, then each neighbour.

## Level 1: the request

Request (§6, Joe, 2026-10-11): deep dives on Alinery and Mastra, "to be able to
demonstrate what's good about my approach while learning from theirs".

- **Q1 Goal.** Demonstrations: for each neighbour, an input, a procedure, and
  what counts as failure, including at least one where the neighbour is expected
  to win (§6). Plus things learned from them. Stated by Joe; no confirmation
  needed.
- **Q2 Method handed over.** The counterpart table (A: Alinery to pattern
  cascades; B: Mastra to War Machine / Agency / futon1b). This is one candidate
  route, not the frame for the others: it answers "where do they resemble us",
  and that is not the same question as "what problem are they solving".
- **Q3 Authority.** For "what problem does X solve", the authority is X's users
  and what they ask for and pay for, not X's documentation. For Mastra this
  means its issues, examples, templates and paid offering (`ee/`). For Alinery
  there are no users in the record (6 stars, repo created 2026-09-10, no
  issues), so the authority is its founders.
- **Q4 Executable.** Both are open source and cloned
  (`~/code/external/alinery` cfc0d1e, `~/code/external/mastra` ebe143c4).
  Cheapest test: run one Mastra template and one Alinery playbook end to end on a
  small real task before reading any architecture document, and log what it took
  and what broke.
- **Q5 Comparator.** The demonstrations will run the same input through FUTON
  and the neighbour. If the neighbour runs and does the job, ask whether FUTON
  could use it rather than compete with it. §3.11 already allows this ("Claude
  Code or Mastra or whatever helps them realize it"). So "build on Mastra" is a
  candidate route alongside "position against Mastra".
- **Q6 Two readings.** For every piece of their code read during a dive, record
  both what FUTON concept it resembles and which user request it answers (an
  issue, a template, a changelog entry). A piece with only the first reading is
  a FUTON-led reading.
- **Q7 Consumer.** The write-ups are read by Joe, by the clients of §3 (client
  problems) and, for Alinery, possibly by its founders. Check each demonstration
  with one of those readers, not with our own sense of it.
- **Q8 Route falsifier.**
  - Alinery "the same thing with a different terminological paint job": wrong if
    a demonstration task can be stated and run equally well in both without
    FUTON's theory supplying a property someone checks.
  - Mastra "solving some of the same problems as the War Machine": wrong if
    Mastra's user demand is about putting agent features into applications
    rather than governing agents that do an operator's work. The issue
    evidence below already leans that way; the dive must confirm or refute it.
  - Review: after the first Q4 run for each neighbour.
- **Q9 Problem.** Joe's: being able to say, with a demonstration, what the
  theory-heavy approach does that the samey SF stack does not, and to adopt what
  they do better.

## Level 2: the neighbours (before reading their docs)

### Mastra

User evidence, gathered 2026-10-11 from the GitHub repo (28,694 stars, created
2024-08-06). The 12 most-upvoted issues, all states:
prompt caching (#3695); Zod v4 (#6043); a supply-chain incident (#18061, #18045);
`mastra dev` in TypeScript monorepos (#1996); AI SDK v5 (#5470); Standard Schema
(#4890); Hono `Context` typing in server middleware (#3094); A2A protocol
(#3677); a deep-research example (#2541); a +17 MB bundle regression blocking
Cloudflare Workers deployments (#3309); registering agents at runtime rather
than from the codebase (#4050).

- Prediction, to test in the dive: Mastra's users are TypeScript developers
  shipping agent features inside their own deployed applications. Their problem
  is integration (schemas, SDK versions, bundling, deployment targets, caching
  cost), and workflows, memory and evals are means to it.
- If so, the overlap with the War Machine is in components (workflows, memory,
  scorers) and not in problem: the War Machine governs agents doing the
  operator's own work.
- Open: what the paid `ee/` directory is for; who pays.

### Alinery

No user evidence in the repo. "A Playbook IDE" (repo description); met at SF
Tech Week, listed "talk to" (10-08 inventory).

- The problem statement has to come from the founders or from running it. Its
  `VISION.md` and whitepaper are its own account of its method. Read them for
  Q2-of-theirs, after the Q4 run, and keep them out of the problem statement.
- Questions to ask them: who was the first user, what were they doing before,
  what made them want playbooks with gates; and which playbook has run most
  often, and on what.

## Not decided here

- Whether either dive is worth the full write-up after its Q4 run.
- Whether "build on Mastra" should become a route; that is Joe's call after the
  Mastra dive.
