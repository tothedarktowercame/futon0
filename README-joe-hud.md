# Joe HUD

The Joe HUD is a personal behavioral dashboard. It doesn't tell you the server
is up — it tells you whether you're doing what you intend to be doing.

## Usage

```
M-x joe-hud          ;; 14-day window (default)
C-u 7 M-x joe-hud    ;; 7-day window
```

Or from the command line:

```
cd ~/code/futon0/scripts && bb -cp . joe-hud.clj [days]
```

## Signals

### 1. Work Schedule

Analyzes git commit timestamps across all futon repos, converted to Central
Time. Shows a histogram by hour-of-day and groups commits into periods:

- **Early morning (5-7)** — pre-work creative window
- **Morning (8-11)** — start of workday
- **Afternoon (12-16)** — core work hours
- **Evening (17-20)** — should be winding down
- **Late night (21-23)** / **Deep night (0-4)** — not ideal

The key metric is **% of commits after 6pm CT**. This is the "am I getting
home on time?" signal. Over 50% means you're working evenings. Under 30%
means you're mostly daytime.

### 2. Evidence Discipline

Queries the futon3c evidence API and computes:

- **Joe turns vs agent turns** — how much are you directing vs delegating?
- **Delegation ratio** — agent turns per joe turn (1:1 is balanced)
- **PSR/PUR/PAR counts** — are you recording pattern selections, outcomes,
  and session reflections?
- **PAR coverage** — what % of sessions end with a Post-Action Review?

The discipline gaps are the loud signal here. Zero PSRs means pattern
selections aren't being recorded. Zero PARs means sessions end without
reflection. The target is 100% PAR coverage.

### 3. Stack Breadth

Counts commits per repo in the window. Surfaces concentration: if 70%+ of
commits are in a single repo, the rest of the stack is languishing.

This is the "am I neglecting things?" signal. Not every repo needs daily
commits, but a healthy stack has activity distributed across concerns.

### 4. Creative Workflow

Reads from `~/code/storage/futon0/recordings/index.json` (when available).
This is a placeholder until laptop metadata sync is wired. The index format:

```json
{
  "recordings": [
    {"title": "morning session", "date": "2026-03-31", "duration": 180}
  ]
}
```

When a recording appears, the HUD detects it. Future: consequences like
"creative boost detected" based on recording frequency.

## Architecture

```
M-x joe-hud
  → emacs/joe-hud.el (async process launcher)
    → bb -cp . joe-hud.clj (babashka runner)
      → futon0/report/joe_hud.clj (signal collection + rendering)
        → git log (commit timestamps)
        → localhost:7070 (futon3c evidence API)
        → recordings/index.json (laptop-synced, optional)
```

All reads, no writes. The HUD is a pure observer (invariant D-I5).

## Data Sources

| Signal | Source | Location |
|--------|--------|----------|
| Work schedule | git commit timestamps | all ~/code/futon* repos |
| Evidence discipline | futon3c evidence API | localhost:7070 |
| Stack breadth | git rev-list --count | all ~/code/futon* repos |
| Creative workflow | recording index | ~/code/storage/futon0/recordings/index.json |

## Future

- **Event-based trigger**: generate a HUD snapshot every N evidence entries
  instead of (or in addition to) on-demand
- **Laptop sync**: rsync/scp recording metadata from laptop to Linode
- **Trend tracking**: save HUD snapshots and compare across weeks
- **Sleep/recovery**: integrate Ultrahuman data if synced
