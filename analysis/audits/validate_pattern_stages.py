"""Check the published labels against evidence joins and actual source text."""
from collections import Counter
import hashlib
import json
from pathlib import Path
import random
import subprocess

from edn_format import loads
from pattern_stage_evidence import plain
from pattern_stage_review import KINDS, STAGES

HERE = Path(__file__).resolve().parent
WORKSPACE = HERE.parents[2]


def main():
    labels = plain(loads((HERE / "pattern-stages-2026-09-21.edn").read_text()))["patterns"]
    priors = plain(loads((HERE / "pattern-stage-dir-priors-2026-09-21.edn").read_text()))["directories"]
    joins = [json.loads(line) for line in (HERE / "pattern-stage-joins-2026-09-21.jsonl").read_text().splitlines()]
    manifest = json.loads((HERE / "pattern-stage-manifest-2026-09-21.json").read_text())
    matched = [row for row in joins if row["status"] == "matched"]
    counts = Counter(row["pattern-id"] for row in matched)
    assert len({r["retrieval-id"] for r in joins}) == len(joins)
    assert all(row["rank1-pattern-ids"] == [row["pattern-id"]] for row in matched)
    assert {row["id"]: row["hits"] for row in labels} == dict(counts)
    assert len(labels) == len({row["id"] for row in labels}) == manifest["labelled-patterns"]
    assert sum(counts.values()) == manifest["matched-retrievals"]
    assert len({r["turn-id"] for r in matched}) == manifest["unique-matched-turns"]
    assert sum(sorted(counts.values(), reverse=True)[:50]) == manifest["top50-hits"]
    source_cache = {}
    def source(row):
        key = (row["path"], row.get("source-revision"))
        if key not in source_cache:
            if key[1]:
                relative = Path(key[0]).relative_to("futon3")
                value = subprocess.check_output(["git", "-C", str(WORKSPACE / "futon3"),
                                                  "show", f"{key[1]}:{relative}"])
            else:
                value = (WORKSPACE / key[0]).read_bytes()
            source_cache[key] = value
        return source_cache[key]
    for row in labels + [r for p in priors for r in p["representatives"]]:
        value = source(row)
        assert hashlib.sha256(value).hexdigest() == row["sha256"], row["id"]
        text = value.decode()
        assert row["id"] in text.splitlines()[row["line"] - 1], (row["id"], row["line"])
        assert row["quote"] in " ".join(text.split()), row["id"]
    for row in labels:
        assert row["kind"] in KINDS and row["stage"] in STAGES
        assert row["confidence"] in ("high", "medium", "low")
        assert row["hits"] >= 1 and row["rationale"]
        assert row["stage-mode"] == {"subject": "topical", "practice": "functional", "mixed": "functional-and-topical"}[row["kind"]]
    root = WORKSPACE / "futon3/library"
    directories = {str(p.relative_to(root)) for p in root.rglob("*") if p.is_dir()
                   and not any(s.startswith(".") for s in p.relative_to(root).parts)}
    assert {r["directory"] for r in priors} == directories
    assert sum(r["flexiarg-files"] for r in priors) == len(list(root.rglob("*.flexiarg")))
    assert sum(r["remaining-flexiarg-files"] for r in priors) == manifest["remaining-flexiarg-files"]
    items = json.loads((HERE / "pattern-stage-blind-items-2026-09-21.json").read_text())
    blind_reference = json.loads((HERE / "pattern-stage-blind-key-2026-09-21.json").read_text())
    key = blind_reference["labels"]
    # Freeze the original exercise when new retrievals expand the labelled cohort.
    population = blind_reference.get("sampling-population", labels)
    label_index = {r["id"]: r for r in labels}
    rng = random.Random(items["seed"])
    expected = []
    for kind in KINDS:
        expected += rng.sample([r for r in population if r["kind"] == kind], 10)
    rng.shuffle(expected)
    assert [r["id"] for r in items["items"]] == [r["id"] for r in expected]
    assert key == {r["id"]: {"kind": label_index[r["id"]]["kind"], "stage": label_index[r["id"]]["stage"]} for r in expected}
    assert not any(k in r for r in items["items"] for k in ("kind", "stage", "hits", "rationale"))
    assert manifest["kind-stage"] == {k: {s: sum(r["kind"] == k and r["stage"] == s for r in labels) for s in STAGES} for k in KINDS}
    assert manifest["stage-hits"] == {s: sum(r["hits"] for r in labels if r["stage"] == s) for s in STAGES}
    print(f"Validated {len(labels)} labels, {sum(counts.values())} hits, {len(joins)} unique retrievals, "
          f"{len(priors)} directories, exact source quotes/hashes/header lines, and all 30 blind cards.")


if __name__ == "__main__":
    main()
