# Kimi seats — the requisition gate

Kimi seats refuse work that does not say what it is for. This is why, how to
comply, and what the gate does with the seat's conversation once it admits you.

Introduced 2026-09-24, after a Kimi key hit its 5-hour limit twice in one
afternoon. The gate lives in `futon3c/src/futon3c/agents/zai_api.clj` and is
shared by the Kimi and Z.ai seats; per-seat settings are in
`futon3c/src/futon3c/agents/kimi_api.clj`.

## The one line

Every call to a Kimi seat must carry, on a line of its own:

```
Requisition: <M-*|E-*|T-*> — <one-line purpose>
```

For example:

```
Requisition: M-futon-seams — interpret operator turn turn-HfZ1oZ
```

The separator may be an em dash, an en dash, `--` or `-`. The line may be
quoted (a forwarded bell often is): a leading `>` is allowed. Repeating the
line is fine as long as every copy names the **same** target.

## Why the gate exists

Every tool round re-sends the whole conversation. A seat that keeps one
session across unrelated dispatches therefore pays for all of them on every
request. On 2026-09-24 the Kimi seats carried one conversation for the whole
day: claude-10's ten dispatches between 13:10 and 15:44 UTC made 310 model
requests carrying **80.3 M input tokens** — 259k per request — and exhausted
the 5-hour quota twice. The record is
`futon3c/holes/labs/kimi-5h-limit-2026-09-24.md`.

The requisition is not paperwork. It is the key the seat uses to decide
whether the conversation it is holding is still relevant to the job in front
of it.

## What the target must be

`M-`, `E-` or `T-` followed by letters, digits, `.`, `_` or `-`. It must
**resolve to a real document**: `holes/<TARGET>.md`, or
`holes/{missions,excursions,tickets}/<TARGET>.md`, in a canonical futon
checkout — a directory matching `futon<N>[a-z]` under `/home/joe/code`.

Worktrees are deliberately excluded. A target that exists only in
`futon3c-my-branch` is not work a seat can be pointed at, because another
agent reading the same name would not find the same document.

If you are clocked in, the refusal message tells you the target your clock
already names, which is usually the answer.

## The four refusals

| reason | what it means |
|---|---|
| `requisition-required` | no line at all |
| `requisition-ambiguous` | two lines naming different targets; name one |
| `requisition-purpose-required` | a target with nothing after the dash |
| `requisition-unresolved` | no such `.md` in a canonical repo, or the name does not match `[MET]-…` |

A refusal is a **failed job**, not a delivery failure. The bell is accepted,
the job runs, and the seat declines — so a caller that only checks whether the
send succeeded will see success. See *Fire-and-forget callers* below.

## Two callers that need no requisition

`auto-bellback` and `parked-resume` continue work the seat already has: the
reply to a bell the seat sent, and a park it set. They inherit the seat's
current target rather than naming one.

## What happens to the conversation

Once a job is admitted, the seat decides what to run it on:

| situation | action |
|---|---|
| nothing carried | keep (fresh) |
| job's target ≠ the conversation's target | **compact** |
| carried tokens ≥ `:cap-tokens` (512k for Kimi) | **compact** |
| same target, under the cap | keep |

Compaction summarises the transcript rather than discarding it. Tool results
over 2,000 characters are cut to head and tail first — "micro compaction" —
because carried context is mostly file reads and command output, and a summary
needs their gist rather than their bytes.

`:cap-tokens` is a placeholder. It clears a same-target conversation that has
grown past the cap, and will be replaced when same-target compaction exists.
k3's context is 1,048,576 tokens, so 512k leaves a long job about 500k to grow
into.

**The practical consequence:** a constant target keeps one warm conversation
and pays no compaction; a target that changes every call pays a summary every
call. Choose the target at the grain of the work, not of the request.

## Sending one

```sh
python3 futon3c/scripts/agency_send.py \
    --to kimi-1 --from <your-id> --kind bell --type request --mode work
```

with the brief on stdin. **`--mode work` is not optional**: without it the
bell is delivered and reported done, and nothing runs.

## Fire-and-forget callers

If you dispatch and do not wait, the requisition gate is invisible to you: the
send exits 0 because the bell was *delivered*, and the refusal happens later
inside the job. A caller that treats exit 0 as success will accumulate work
that never happened.

`futon3c`'s turn-analysis dispatcher is the worked example. It left 152
records at `requested` — indistinguishable from a busy seat — before it was
taught to record the job id from the bell response and ask afterwards what
became of it (`futon3c/scripts/turn_dispatch_reap.py`). If you write a
fire-and-forget caller, record the job id.

## When a fix does not take

Emacs-side callers hold their dispatch function in memory. Editing the `.el`
changes nothing until it is loaded:

```sh
emacsclient -e '(load-file "/home/joe/code/futon3c/emacs/session-turn-analysis.el")'
```

Verify against the running process, not the file. On 2026-09-24 a corrected
dispatcher sat on disk through two further refusals because nothing had
loaded it.
