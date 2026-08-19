#!/usr/bin/env python3
"""Is the behind-the-desk shelf actually on this desk?

Reads data/library.json and checks that every `reference`-tier item is present
here. Reference tier means present on EVERY host, so a missing one is a finding,
not a preference.

Why this is not part of futon-sync: futon-sync reports drift in TRACKED files.
Every item on this shelf is invisible to it by construction -- gitignored, or
not a repo, or a repo with no remote. A clean `futon-sync` says nothing about
whether the shelf is stocked.

Exit 0 = shelf complete. Exit 1 = something is missing or the ceiling is blown.
"""
import json, os, sys

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
CAT = os.path.join(os.path.dirname(__file__), "..", "data", "library.json")

def du(p):
    if os.path.isfile(p):
        return os.path.getsize(p)
    t = 0
    for dp, _, fs in os.walk(p):
        for f in fs:
            try: t += os.path.getsize(os.path.join(dp, f))
            except OSError: pass
    return t

cat = json.load(open(CAT))
ref = [i for i in cat["items"] if i["tier"] == "reference"]
missing, total = [], 0
print("behind-the-desk shelf — %d reference items" % len(ref))
for i in ref:
    p = os.path.join(ROOT, i["path"])
    if os.path.exists(p):
        b = du(p); total += b
        print("  PRESENT  %-42s %7.0f KB" % (i["path"], b / 1024))
    else:
        missing.append(i)
        print("  MISSING  %-42s          <-- %s" % (i["path"], i["purpose"][:60]))

ceiling = cat["ceiling_bytes"]
print("  shelf: %.1f MB of %.0f MB ceiling (%.1f%%)"
      % (total / 1048576, ceiling / 1048576, 100.0 * total / ceiling))

# A catalogue earns its keep by covering what you do NOT hold.
other = [i for i in cat["items"] if i["tier"] != "reference"]
print("  catalogued but not shelved: %d items (%s)"
      % (len(other), ", ".join(sorted({i["tier"] for i in other}))))

if missing:
    print("\n%d reference item(s) MISSING on this host." % len(missing))
    for i in missing:
        print("  %s  -- masters: %s" % (i["path"], ", ".join(i.get("masters", []))))
    sys.exit(1)
if total > ceiling:
    print("\nShelf exceeds its ceiling. Promote something off it, or raise the "
          "ceiling deliberately -- 'behind the desk' means it fits behind the desk.")
    sys.exit(1)
