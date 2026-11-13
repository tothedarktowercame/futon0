# Futon Workshop — UK Reproducibility Network

## Part 1: The Experiments (< 100 words)

**Futon3c: Multi-agent coordination with full capture**

Two humans and two AI agents (Claude, Codex) share an IRC channel backed by a persistent evidence store (XTDB). Agents coordinate via @mentions: Codex scopes tasks from mission docs and delegates to Claude via GitHub issues; Claude reviews and feeds back. A watchdog agent (Tickle) orchestrates overnight batch runs and detects stalls. Every message, tool invocation, and review verdict is captured as typed, queryable evidence. In a two-session experiment, this produced 17 PR reviews across 85 proposals with measurable cross-batch quality improvement. The infrastructure is ordinary; the combination -- multiple humans, multiple agents, shared codebase, complete audit trail -- is new.

---

## Part 2: The Open Inquiry System (< 100 words)

**Missions, evidence, and peripherals: development as open inquiry**

Leibniz dreamed of *calculemus* -- disputes settled by making reasoning mechanical and transparent. Futonic missions pursue this for software. Each task requires agents to record *why* they chose an approach, *whether* it worked, and *what* they learned -- typed evidence entries in a persistent, queryable store. Peripherals (IRC, Emacs, proof steppers, watchdog bots) are surfaces where work happens; each emits evidence automatically. The development process doesn't produce a research record as a side-effect. It *is* the research record: every design decision, pattern selection, and review verdict captured with provenance, queryable by any researcher after the fact.

---

## Part 3: Interactive Session Design

### #futon: A One-Hour Encounter

**Format:** 60 minutes, up to 20 participants, no slides, no facilitation deck.
**Materials:** One printed card per seat (IRC connection details + three bang commands). One projector showing a live `curl` of the evidence API (optional -- can also be a shared URL).

---

### Before the room (host prep)

- futon3c running on Linode with ngircd, ngircd-bridge, Claude + Codex registered
- `INVOKE_SKIP_WHEN_BUSY=1` (default) -- agents respond to one @mention at a time, others get a "busy" notice. This is a feature: 20 humans, 2 agents, real contention.
- Confirm TLS on port 6697 for external clients
- Set `!mission focus` on a real open mission (e.g. M-tickle-overnight or a fresh one scoped for the session)
- Prepare a seed question -- something genuinely open about the system. Examples:
  - *"This system claims every design decision is traceable. Can you find one that isn't?"*
  - *"Two agents reviewed 85 proposals overnight. What can you tell about review quality from the evidence trail alone?"*
  - *"The evidence store has ~2000 entries from the last month. What's missing?"*

**Card on table:**

```
Join: irc.futonproject.org:6697 (TLS) or port 6667
      Channel: #futon  |  Password: [on card]
      Phone: Igloo IRC app works fine

Three commands to know:
  !help           -- see what's available
  @claude <msg>   -- talk to Claude
  @codex <msg>    -- talk to Codex
```

Nothing else on the card. No instructions, no objectives, no agenda.

---

### 0:00 -- 0:08 | Arrive

People sit down, read the card, connect. The host is already in #futon and types:

> **joe:** welcome to #futon. you're joining a live coordination channel -- two AI agents (claude, codex) and whoever else is here. everything said here is captured as typed, timestamped evidence. take a minute to connect and say hello.

No preamble. No "today we'll be exploring..." The channel is already alive -- there may be residual messages from earlier work. That's intentional. They're joining something in progress, not a clean demo environment.

Claude and Codex are gated (default @mention-only). The agents don't greet people automatically -- they respond when spoken to.

**If someone asks "what is this?"** -- they can `@claude what is this channel?` and get an answer from the system itself. The host doesn't explain.

---

### 0:08 -- 0:12 | The question

Host posts the seed question. One message. Example:

> **joe:** here's a question for the room. this system recorded 17 PR reviews overnight, run by agents with a human in Houston relaying when things stalled. the evidence is in the store. @claude how many evidence entries exist from the tickle CT review sessions?

Claude responds with a count or summary. This does two things: (a) demonstrates that the agents are real and responsive, (b) seeds a concrete inquiry with real data to look at.

Host follows with:

> **joe:** the question for the next 35 minutes: **what can you learn about what happened, using only what's in this channel and the evidence store?** talk to each other, talk to the agents, use !mc commands. there's no right answer.

---

### 0:12 -- 0:47 | The work

**The host does almost nothing.** This is the core of the session. What happens:

- People start @mentioning Claude and Codex. They hit the busy queue. They discover contention is real -- 20 people, 2 agents. They start coordinating with *each other* to avoid redundant questions. This is the point.
- Some people try `!mc missions` or `!mc review` and discover the portfolio structure.
- Some people ask Claude to explain specific evidence entries.
- Some people start talking to each other in channel about what they're finding.
- The agents' responses appear in the same stream as human messages. After 10 minutes, the boundary between "human contribution" and "agent contribution" becomes blurry in the scrollback.

**Host interventions (only if needed):**

- If the channel goes quiet for >2 minutes: post a small provocation. ("@codex what was the most common reason for REQUEST_CHANGES in the CT reviews?")
- If one person is monopolising agent attention: say nothing. Others will notice the busy messages and self-organise. If they don't, that's also data.
- If someone asks "what are we supposed to be doing?": "finding out what you can learn from the evidence. !help shows the tools."

**What the host should NOT do:**
- Steer toward a "correct" finding
- Summarise what people are discovering
- Explain how the system works (the system explains itself when asked)
- Call on people or manage turns

The session works because the infrastructure is real and the contention is real. Twenty people trying to coordinate with two agents in a single channel is not a simulation of distributed work -- it *is* distributed work. The friction (busy queues, agents that take 30 seconds to respond, messages scrolling past) is genuine.

---

### 0:47 -- 0:55 | The mirror

Host runs a live query against the evidence store -- everything captured in the last hour:

> **joe:** @claude how many evidence entries were created in the last 60 minutes? who are the authors?

Or via curl on the projector:

```bash
curl -s 'http://localhost:7070/api/alpha/evidence?limit=200' | \
  jq '[.[] | select(.at > "2026-03-05T14:00")] |
      {entries: length,
       authors: [.[].author] | unique,
       types: [.[].type] | group_by(.) | map({type: .[0], count: length})}'
```

The output shows: N evidence entries. Authors include `claude-1`, `codex-1`, and every human nick in the room. Types include `forum-post` (their IRC messages), agent responses, coordination events.

Host says one thing:

> **joe:** that's the last 55 minutes. every message you typed, every agent response, every !mc query -- captured as typed evidence with author, timestamp, and reply chain. this is what an automatically-generated research record looks like. the same store holds the overnight agent reviews, the mission portfolio, and now your session.

Pause. Let them look at it.

---

### 0:55 -- 1:00 | The question you didn't have

> **joe:** last thing. type one question into #futon -- something you're genuinely wondering now that you weren't wondering an hour ago. not feedback, not a comment. a question.

People type their questions. The agents are still live -- if someone @mentions claude with their question, Claude might answer it, which is fine. The questions become evidence entries too.

The host does not summarise, does not pick favourites, does not synthesise themes. The questions sit in the channel and the evidence store. Anyone can query them later.

> **joe:** those questions are now in the evidence store too. thanks for being here.

---

### What makes this work

**No skills transferred.** Nobody learned IRC or evidence queries or mission control. They used them, in the way you use a door handle -- instrumentally, without studying door-handle design.

**The question emerges from the experience.** Likely questions (from past encounters with similar systems):
- "Who authored the finding -- the human who asked the question or the agent who answered it?"
- "If everything is captured, what *isn't* in the evidence trail?"
- "Could I reproduce this session from the evidence alone?"
- "When the agent was busy and I asked someone else instead, was that coordination or just frustration?"
- "What happens to this evidence now?"

These are genuine reproducibility questions that you can't get from a talk about reproducibility. They come from having been inside the system.

**The contention is pedagogical.** Twenty people and two agents in one channel is deliberately under-resourced. The bottleneck forces human-human coordination to emerge. If there were twenty agents, everyone would talk to their own agent and the session would be twenty parallel conversations. The scarcity makes it social.

**No facilitation theatre.** No breakout groups, no report-backs, no sticky notes, no "what did we learn?" round. The channel is the session. The evidence store is the record. The questions at the end are the only output.

---

### Logistics checklist

| Item | Detail |
|------|--------|
| Server | futon3c on Linode, ngircd + bridge running |
| Ports | 6667 (plaintext, LAN), 6697 (TLS, external) |
| DNS | Point a friendly hostname at the server |
| Agents | Claude + Codex registered, bridge running, INVOKE_SKIP_WHEN_BUSY=1 |
| Load test | Test with 5 concurrent IRC clients beforehand |
| Fallback | If an agent crashes, `!reset <agent-id>` from host. Have Drawbridge REPL open. |
| Evidence query | Pre-test the curl/jq pipeline for the mirror phase |
| Cards | Print connection details. No QR codes to apps -- just server:port and channel. |
| Projector | Optional. For the mirror phase only. Can also just share a URL. |
| Wifi | Confirm the venue wifi doesn't block IRC ports. Bring a 4G hotspot as backup. |
