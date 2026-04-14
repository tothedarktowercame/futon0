# Excursion: Drawbridge CIDER Adapter

**Date:** 2026-04-13
**Entry point:** M-daily-scan internal scan → upstream dependency network → identified gap

No existing CIDER middleware for Drawbridge. Would allow `M-x cider-connect`
to a Drawbridge HTTP nREPL endpoint directly. The stack uses Drawbridge daily
(futon3c hot-reload). 2-3 days effort, permanent consulting signal. First
exercise of `depositing/upstream-patch-signal` pattern.
