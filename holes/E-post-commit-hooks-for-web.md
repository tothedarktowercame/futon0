# Excursion: E-post-commit-hooks-for-web

**Date:** 2026-08-22
**Status:** IDEA — deliberately not built (Joe: "let's not do it yet")
**Owner:** unassigned
**Entry point:** claude-13 + Joe, emacs-repl, while publishing two papers to
`zone.hyperreal.enterprises/wip/` by hand.
**Relates to:** `M-futon-problems` (G1, D7) · `war-room/wr-8-typed-files-are-sources-of-truth`

---

## The idea

A post-commit hook that rebuilds and republishes the works-in-progress site, so
`zone.hyperreal.enterprises/wip/` is a projection of committed state rather than
of whoever last ran the pipeline by hand.

## Why it is worth doing

The site is currently a **rendered** artefact: correct at publish time, and
decaying from then on. Everything the mission has to say about stale surfaces
applies to it — six strategic views in this workspace assert continuous currency
at a median age of thirty-six days, and none of them is wrong about anything
except how old it is. A published paper that silently lags its source is the
seventh.

`WR-8` already rules on the general case: typed files are the source of truth,
prose is regenerated. A hook is that ruling applied to the web surface. `D7` in
`M-futon-problems` states the closure test: *a gap is closed only by an artefact
that re-derives.* A hand-published site does not re-derive; a hooked one cannot
help it.

## What makes it feasible now

The pipeline became five commands today, with no manual steps between them:

```
*.tex + *.edn
  → bb empirics-futon/gen_*.bb        (counts, tables, figure overlays)
  → ./build-site.sh                    (latexml-oxide; guard requires it)
  → tuftify.py                         (measure, sidenotes, figure placement)
  → publish-wip.py                     (site-local overrides, sort widget)
  → /var/www/zone.hyperreal.enterprises/wip/
```

`build-site.sh` is parameterised — `SRC`, `BIB`, `DOCS_SPEC`, `PRELOAD`,
`SITE_HOME_LABEL`, `SITE_HOME_HREF` — so it builds any paper without being
copied, and the shared fixes now live in `tuftify.py` where every paper picks
them up.

## Design sketch

- **Where the hook lives.** Two candidate repos publish to the same docroot
  (`p4ng` for futon-2026, `futon5` for the CA paper). Either a hook in each, or
  one script both call. Prefer the latter: two hooks that drift produce a site
  half-built from each.
- **What it runs.** Regenerate seams, build, tuftify, publish, for the documents
  that repo owns. Not the whole site — a commit to p4ng should not rebuild the
  CA paper.
- **Where it runs.** Zone only. It is the box with TeX Live 2026 and the docroot
  (`/var/www/zone.hyperreal.enterprises`, owned by `joe`, no sudo needed). A
  hook on a laptop would silently do nothing useful.

## Wrinkles, in the order they will bite

1. **Per-document `PRELOAD` is not parameterised.** futon-2026 needs
   `p4ng-html-shim.sty`, the CA paper needs `apa7-html-shim.sty`, and
   `DOCS_SPEC` carries no preload field. Today that means two invocations. The
   fix is a fifth field in `DOCS_SPEC`; until then a single-build hook cannot
   do both papers.
2. **The build is slow.** A full oxide conversion of futon-2026 runs past 115s
   and the CA paper's six documents take longer. A blocking `post-commit` hook
   that pauses a commit for minutes will be disabled by the first person it
   annoys. Detach it, or move to `post-receive` on a push.
3. **Partial publication.** The publish step copies HTML and assets separately.
   A build that fails midway can leave a page referencing figures that were
   never copied. Build to a staging directory and swap, rather than writing into
   the live docroot.
4. **The docroot is not versioned.** `/var/www` is a plain directory, so a bad
   publish has no history to roll back to. Either version it or accept that the
   pipeline is the only recovery path — which is fine *if* the pipeline is
   committed, which it now is.

## The failure mode to design against

A hook that fails quietly is worse than no hook. It converts "the site is stale
and nobody knows when it was built" into "the site is stale and everyone
believes it is current" — the same upgrade in confidence, without the upgrade in
accuracy, that this workspace has now found six times in one day (`state: done`
over `execution.executed: false`; star titles reading n=0 over positions
reading n=1–2; a busy-store 503 counted as zero results).

So the hook must leave a mark the site itself carries. The minimum:

- **a build stamp in the page** — source commit sha and build time, visible in
  the footer, so a reader can tell what they are looking at without asking;
- **loud failure** — non-zero exit reported somewhere a human sees, not just in
  a log nobody reads;
- **no silent partial** — staging directory, atomic swap.

The Uxbridge plotting table did this with a five-minute colour clock: a plot
laid three colours ago was *visibly* three colours old, and staleness was a
property of the artefact rather than something a controller had to remember to
check. A build stamp is the same device.

## Not doing it yet, and why that is fine

The pipeline works and the papers are published. Automating it now would
automate a process that is still changing shape weekly — the parameterisation
moved three times today, and the fixes moved repos twice. Hooks are worth
installing once the thing they invoke has stopped moving.

**Next step when it is time:** the fifth `DOCS_SPEC` field (per-document
preload), because every other wrinkle has a known answer and that one is a
blocker.
