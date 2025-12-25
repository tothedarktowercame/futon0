# Boundary Notes

## What It Is

The boundary snapshot is a repo-local `boundary.edn` file that reports which
items are still "inside" (state or filesystem) versus fully persisted. Futon3
uses it to populate Stack HUD boundary gaps, and the standalone
`scripts/boundary_hud.el` view can display it directly.

## File Location

- `boundary.edn` lives at the repo root, alongside `deps.edn`.
- Each repo can write its own snapshot; Futon3 merges them by `:id`.

Example shape:

```clojure
{:generated_at "2025-12-19T02:03:04.000Z"
 :futons [{:id "f4"
           :exists true
           :last_modified "2025-12-19T01:58:10.000Z"
           :prototypes 12
           :missing_evidence 7
           :todo_count 0
           :lab_raw_count 3
           :lab_stub_count 3
           :lab_doc_draft_count 1
           :media_unpersisted 6}]}
```

## Boundary Registry

- `futon4/dev/boundary-generate.clj` writes `boundary.edn` with lab and media
  counts. It scans `lab/raw`, `lab/stubs`, `lab/doc-drafts` and (when present)
  a zoom media index (`~/code/storage/zoomr4/meta/zoom_sync_index.json`).
- `futon3/src/futon3/stack/status.clj` loads `boundary.edn` from multiple repos
  and merges entries by `:id` for the Stack HUD.
- `scripts/boundary_hud.el` is a standalone HUD that reads a single
  `boundary.edn` snapshot (defaulting to the current repo root).

## Updating

From the repo root, generate a snapshot (Clojure):

```
clojure -Sdeps '{:deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' \
  -M dev/boundary-generate.clj
```

Pass explicit paths if needed:

```
clojure -Sdeps '{:deps {org.clojure/data.json {:mvn/version "2.5.0"}}}' \
  -M dev/boundary-generate.clj \
  --repo-root /path/to/repo --output /path/to/repo/boundary.edn \
  --media-index /path/to/zoomr4/meta/zoom_sync_index.json
```

If a repo requires heavier computation, use a full Clojure entry point instead
of bb, but keep the output format compatible with the schema above.
