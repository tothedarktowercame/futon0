#!/usr/bin/env python3
"""Mine recurring step sequences from LeanEuclid Book I proofs.

Reads ~/code/LeanEuclid/Book/Prop*.lean (read-only), parses each theorem
into an ordered list of tactic steps, and mines:

  * contiguous n-grams of steps (n = 2..5), at exact-name level and at
    class level;
  * data-flow chains (length 2..4 steps) along def-use edges: step B uses
    an object bound by an earlier step A's `as` clause (the latest binder of
    that name). Edges follow data flow, so the order of independent steps
    between A and B does not matter.

Support = number of DISTINCT propositions a motif occurs in; primed
variants (proposition_42'', proposition_42''') count as one proposition. Motifs with support >= MIN_SUPPORT are reported. Outputs JSON and a
human-readable markdown report.
"""

import json
import re
import sys
from collections import defaultdict
from pathlib import Path

DEFAULT_BOOK_DIR = Path.home() / "code" / "LeanEuclid" / "Book"
OUT_DIR = Path(__file__).resolve().parents[1] / "holes" / "excursions" / "E-euclid-tutor-data"
MIN_SUPPORT = 3

# ---------------------------------------------------------------------------
# Name -> class table. Edit here.
# ---------------------------------------------------------------------------

CLASS_TABLE = {
    # construct-line
    "line_from_points": "construct-line",
    # construct-circle
    "circle_from_points": "construct-circle",
    # intersect
    "intersection_lines": "intersect",
    "intersection_circles": "intersect",
    "intersection_circle_line_extending_points": "intersect",
    "intersection_circle_line_between_points": "intersect",
    "intersections_circle_line": "intersect",
    "intersection_same_side": "intersect",
    "intersection_opposite_side": "intersect",
    # extend
    "extend_point": "extend",
    "extend_point_longer": "extend",
    "extend_point_shorter": "extend",
    # metric / angle / area axioms and facts
    "sum_angles_onlyif": "metric/angle/area",
    "sum_angles_if": "metric/angle/area",
    "sum_areas_if": "metric/angle/area",
    "sum_parallelograms_area": "metric/angle/area",
    "rectangle_area": "metric/angle/area",
    "parallelogram_area": "metric/angle/area",
    "point_between_points_shorter_than": "metric/angle/area",
    "between_if": "metric/angle/area",
    "point_on_circle_onlyif": "metric/angle/area",
    "point_on_line_same_side": "metric/angle/area",
    "perpendicular_if": "metric/angle/area",
    "superposition": "metric/angle/area",
    "distinct_points": "metric/angle/area",
}

PROPOSITION_RE = re.compile(r"^proposition_\d+'*$")

OTHER_NAMES = set()  # filled at classify() time for the report


def classify(name):
    if PROPOSITION_RE.match(name):
        return "cite-proposition"
    if name in CLASS_TABLE:
        return CLASS_TABLE[name]
    OTHER_NAMES.add(name)
    return "other"


# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

THEOREM_RE = re.compile(r"^theorem\s+([\w']+)")
# Tactic heads that start a step. Lines inside a theorem that begin with one
# of these (optionally after a focus dot `.` / `·`) start a new step.
TACTIC_HEADS = (
    "euclid_apply",
    "euclid_finish",
    "euclid_intros",
    "euclid_assert",
    "use",
    "by_contra",
    "by_cases",
    "have",
    "constructor",
    "exists",
    "split_ors",
    "assumption",
)
STEP_RE = re.compile(
    r"^(\s+)[.·]?\s*(" + "|".join(TACTIC_HEADS) + r")\b(.*)$")
IDENT_RE = re.compile(r"[A-Za-z_][\w']*")
AS_RE = re.compile(r"\bas\s+(?:\(([^)]*)\)|([A-Za-z_][\w']*))")
STRUCTURAL_RE = re.compile(r"^(theorem\b|end\b|/--|namespace\b|import\b)")


def parse_theorems(text, filename="<memory>"):
    """Split a .lean text into theorems; each theorem is a list of steps."""
    # Drop block comments /-- ... -/ so commented-out proofs are not parsed.
    text = re.sub(r"/--.*?-/", "", text, flags=re.S)
    lines = text.splitlines()
    theorems = []
    current = None  # (name, steps)
    i = 0
    while i < len(lines):
        line = lines[i]
        m = THEOREM_RE.match(line)
        if m:
            if current:
                theorems.append(current)
            current = {"name": m.group(1), "file": filename, "steps": []}
            i += 1
            continue
        if current is None:
            i += 1
            continue
        sm = STEP_RE.match(line)
        if sm:
            tactic, rest = sm.group(2), sm.group(3)
            # join continuation lines: while parentheses are unbalanced, or
            # (for multi-line tactics like `use foo a b` / `euclid_assert`)
            # while the next indented line does not start a new step or
            # structural line.
            while i + 1 < len(lines):
                nxt = lines[i + 1]
                if rest.count("(") > rest.count(")"):
                    rest += " " + nxt.strip()
                    i += 1
                    continue
                if tactic in ("use", "euclid_assert") and nxt.startswith(" ") \
                        and not STEP_RE.match(nxt) \
                        and not STRUCTURAL_RE.match(nxt.strip()) and nxt.strip():
                    rest += " " + nxt.strip()
                    i += 1
                    continue
                break
            step = {"line": i + 1, "tactic": tactic}
            if tactic == "euclid_apply":
                pm = re.match(r"\s*(\()?\s*([\w']+)(.*)$", rest)
                if not pm:
                    raise ValueError(f"{filename}:{i+1}: unparsable euclid_apply: {rest!r}")
                step["name"] = pm.group(2)
                call_text = pm.group(3)
                if pm.group(1):  # parenthesised form: strip trailing ')'
                    call_text = call_text.rstrip().rstrip(")")
                am = AS_RE.search(call_text)
                if am:
                    if am.group(1) is not None:
                        bindings = [b.strip() for b in am.group(1).split(",") if b.strip()]
                    else:
                        bindings = [am.group(2)]
                    call_text = call_text[: am.start()]
                    step["bindings"] = bindings
                else:
                    step["bindings"] = []
                step["uses"] = set(IDENT_RE.findall(call_text)) - {step["name"]}
                step["class"] = classify(step["name"])
            else:
                step["name"] = tactic
                step["bindings"] = []
                step["uses"] = set()
                step["class"] = classify(tactic)
            current["steps"].append(step)
        elif line.startswith("end ") or line.startswith("end\n") or line == "end":
            if current:
                theorems.append(current)
                current = None
        i += 1
    if current:
        theorems.append(current)
    return theorems


def parse_book(book_dir=DEFAULT_BOOK_DIR):
    files = sorted(Path(book_dir).glob("Prop*.lean"))
    theorems = []
    for f in files:
        theorems.extend(parse_theorems(f.read_text(), str(f)))
    return theorems


# ---------------------------------------------------------------------------
# Motif mining
# ---------------------------------------------------------------------------


def ngrams(steps, n, key):
    return [tuple(s[key] for s in steps[i : i + n]) for i in range(len(steps) - n + 1)]


def dataflow_edges(steps):
    """Def-use edges: (i, j) means step j uses a name whose most recent
    binding before j was made by step i (so i < j always). Source order of
    independent steps does not matter because edges follow data flow, not
    adjacency; but a use can only refer to an earlier binding. Shadowed
    names (rebound in a later step or case branch) resolve to the latest
    binder. Only euclid_apply steps bind names."""
    edges = []
    latest = {}  # name -> index of the step that most recently bound it
    for j, b in enumerate(steps):
        for i in sorted({latest[n] for n in b["uses"] if n in latest}):
            edges.append((i, j))
        for n in b["bindings"]:
            latest[n] = j
    return edges


def chains(steps, min_len=2, max_len=4):
    """All simple step sequences of length min_len..max_len along def-use
    edges. Returns a list of tuples of step indices."""
    edges = dataflow_edges(steps)
    adj = defaultdict(list)
    for i, j in edges:
        adj[i].append(j)

    out = []

    def dfs(path):
        if len(path) >= min_len:
            out.append(tuple(path))
        if len(path) == max_len:
            return
        for j in adj[path[-1]]:
            if j not in path:
                dfs(path + [j])

    starts = sorted(set([i for i, _ in edges] + [j for _, j in edges]))
    for s in starts:
        dfs([s])
    return out


def proposition_of(theorem_name):
    """Support counts propositions, not theorems: proposition_42'' and
    proposition_42''' are variants of I.42 and share most of their steps."""
    return theorem_name.rstrip("'")


def mine(theorems, min_support=MIN_SUPPORT):
    """Returns motifs dict: kind -> level -> {motif: {support, examples}}."""
    motifs = {
        "ngram": {"name": defaultdict(set), "class": defaultdict(set)},
        "chain": {"name": defaultdict(set), "class": defaultdict(set)},
    }
    examples = {
        ("ngram", "name"): defaultdict(list),
        ("ngram", "class"): defaultdict(list),
        ("chain", "name"): defaultdict(list),
        ("chain", "class"): defaultdict(list),
    }
    for th in theorems:
        steps = th["steps"]
        loc = f'{Path(th["file"]).name}:{steps[0]["line"]}' if steps else th["file"]
        for n in range(2, 6):
            for key in ("name", "class"):
                for g in ngrams(steps, n, key):
                    motifs["ngram"][key][g].add(proposition_of(th["name"]))
        for path in chains(steps):
            for key in ("name", "class"):
                g = tuple(steps[i][key] for i in path)
                motifs["chain"][key][g].add(proposition_of(th["name"]))
                ex = (
                    f'{Path(th["file"]).name}:{steps[path[-1]]["line"]}'
                )
                if len(examples[("chain", key)][g]) < 3:
                    examples[("chain", key)][g].append(ex)
    # attach examples to ngram motifs
    for th in theorems:
        steps = th["steps"]
        for n in range(2, 6):
            for key in ("name", "class"):
                for i, g in enumerate(ngrams(steps, n, key)):
                    if len(examples[("ngram", key)][g]) < 3:
                        examples[("ngram", key)][g].append(
                            f'{Path(th["file"]).name}:{steps[i]["line"]}'
                        )
    result = {}
    for kind, levels in motifs.items():
        result[kind] = {}
        for key, d in levels.items():
            result[kind][key] = {
                " -> ".join(g): {
                    "support": len(props),
                    "propositions": sorted(props),
                    "examples": examples[(kind, key)][g],
                }
                for g, props in d.items()
                if len(props) >= min_support
            }
    return result


# ---------------------------------------------------------------------------
# Reporting
# ---------------------------------------------------------------------------


def i5_check(motifs, theorems):
    """Look for the known I.5 sequence: extend, cut off equal via
    proposition_3, join, proposition_4 twice."""
    found = []
    # exact name-level contiguous n-gram
    want_names = (
        "extend_point",
        "proposition_3",
        "line_from_points",
        "proposition_4",
        "proposition_4",
    )
    got = motifs["ngram"]["name"].get(" -> ".join(want_names))
    found.append(("ngram name-level 5-gram " + " -> ".join(want_names), got))
    # class-level chain: extend, cite-proposition, construct-line, cite x2
    want_classes = (
        "extend",
        "cite-proposition",
        "construct-line",
        "cite-proposition",
        "cite-proposition",
    )
    got = motifs["chain"]["class"].get(" -> ".join(want_classes))
    found.append(("chain class-level " + " -> ".join(want_classes), got))
    got = motifs["ngram"]["class"].get(" -> ".join(want_classes))
    found.append(("ngram class-level " + " -> ".join(want_classes), got))
    # relaxed: the I.5 core without the leading extend
    core = ("proposition_3", "line_from_points", "proposition_4", "proposition_4")
    got = motifs["ngram"]["name"].get(" -> ".join(core))
    found.append(("ngram name-level core " + " -> ".join(core), got))
    core_c = ("cite-proposition", "construct-line", "cite-proposition", "cite-proposition")
    got = motifs["chain"]["class"].get(" -> ".join(core_c))
    found.append(("chain class-level core " + " -> ".join(core_c), got))
    return found


def render_md(motifs, theorems, i5, parse_count):
    lines = ["# LeanEuclid Book I step motifs", ""]
    lines.append(f"Parsed {len(theorems)} theorems, {parse_count} euclid_apply steps, "
                 f"{sum(len(t['steps']) for t in theorems)} steps total (all tactics).")
    lines.append("")
    lines.append("Support = number of distinct propositions (primed variants of one "
                 "proposition count once) the motif occurs in; threshold >= "
                 f"{MIN_SUPPORT}. Data-flow chains follow def-use edges from each use "
                 "to the latest earlier binder of the name.")
    lines.append("")
    for kind in ("ngram", "chain"):
        for key in ("class", "name"):
            section = motifs[kind][key]
            lines.append(f"## {kind} ({key} level), top by support")
            lines.append("")
            lines.append("| motif | support | examples |")
            lines.append("|---|---|---|")
            top = sorted(section.items(), key=lambda kv: (-kv[1]["support"], kv[0]))[:20]
            for motif, info in top:
                ex = ", ".join(info["examples"])
                lines.append(f"| {motif} | {info['support']} | {ex} |")
            lines.append("")
    lines.append("## The I.5 sequence (extend, cut off equal, join, SAS twice)")
    lines.append("")
    for label, got in i5:
        if got:
            lines.append(f"- FOUND as {label}: support {got['support']} "
                         f"({', '.join(got['propositions'])})")
        else:
            lines.append(f"- not found as {label}")
    lines.append("")
    lines.append("## Names classed as 'other'")
    lines.append("")
    for n in sorted(OTHER_NAMES):
        lines.append(f"- {n}")
    lines.append("")
    return "\n".join(lines)


def main(argv):
    book_dir = Path(argv[1]) if len(argv) > 1 else DEFAULT_BOOK_DIR
    theorems = parse_book(book_dir)
    parse_count = sum(
        1 for t in theorems for s in t["steps"] if s["tactic"] == "euclid_apply"
    )
    motifs = mine(theorems)
    i5 = i5_check(motifs, theorems)
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    payload = {
        "parse_count": parse_count,
        "theorems": len(theorems),
        "min_support": MIN_SUPPORT,
        "motifs": motifs,
    }
    (OUT_DIR / "step-motifs.json").write_text(json.dumps(payload, indent=1))
    (OUT_DIR / "step-motifs.md").write_text(
        render_md(motifs, theorems, i5, parse_count) + "\n"
    )
    print(f"theorems: {len(theorems)}  euclid_apply steps parsed: {parse_count}")
    for kind in ("ngram", "chain"):
        for key in ("class", "name"):
            print(f"{kind}/{key}: {len(motifs[kind][key])} motifs at support>={MIN_SUPPORT}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
