#!/usr/bin/env python3
"""One authority for publication Clean and bounded two-hub recovery."""

from datetime import datetime, timezone
import json
from pathlib import Path
import subprocess
import sys

ROOT = Path(__file__).parents[1]
STATE = Path.home() / ".local/state/futon-clean/latest.json"
MAX_RECEIPT_AGE_SECONDS = 30 * 60


def main():
    gate = subprocess.run(
        ["bb", str(ROOT / "scripts/futon-sync.clj"), "check-clean", "--json"],
        text=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    try:
        publication = json.loads(gate.stdout)
    except json.JSONDecodeError:
        publication = {"clean": False, "error": "publication-gate-invalid-json",
                       "stderr": gate.stderr.strip()}
    if not STATE.exists():
        recovery = {"ok": False, "reason": "worker-receipt-missing"}
    else:
        receipt = json.loads(STATE.read_text())
        at = datetime.fromisoformat(receipt["at"])
        age = max(0, (datetime.now(timezone.utc) - at).total_seconds())
        rows = receipt.get("recovery", {}).get("repos", [])
        recovery = {
            "ok": bool(receipt.get("recovery", {}).get("ok"))
                  and age <= MAX_RECEIPT_AGE_SECONDS
                  and all(r.get("post-commit-hook-installed") for r in rows),
            "receipt-age-seconds": round(age, 3),
            "max-receipt-age-seconds": MAX_RECEIPT_AGE_SECONDS,
            "repo-count": len(rows),
            "missing-hooks": [r["path"] for r in rows
                              if not r.get("post-commit-hook-installed")],
            "hub-failures": [{"repo": r["repo"], **h} for r in rows
                             for h in r.get("hubs", []) if not h.get("ok")],
            "scope": receipt.get("recovery", {}).get("scope"),
        }
    report = {"clean": bool(publication.get("clean")) and recovery["ok"],
              "at": datetime.now(timezone.utc).isoformat(),
              "publication": publication, "recovery": recovery}
    print(json.dumps(report, indent=2, sort_keys=True))
    return 0 if report["clean"] else 1


if __name__ == "__main__":
    sys.exit(main())
