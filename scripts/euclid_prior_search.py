#!/usr/bin/env python3
"""Prior-ordered search over the System E engine, vs the uninformed BFS.

Prior (documented formula): a candidate move's rule r scored given the rule p
of the previous step (START for the first move), from data-flow-chain motifs
mined by euclid_step_motifs.mine on ONLY the theorems of propositions < n
(no leakage; re-mined per target, min_support=1 so raw proposition counts are
kept):

  score(r | p) = log(1 + N_name(p -> r)) + beta * log(1 + N_class(pc -> rc))

  * name level: N_name(p -> r) = support (number of distinct propositions)
    of the chain motif (p, r) at name level; back-off term at class level
    (pc, rc) with weight beta = 0.5, added (not replaced) so the name signal
    can be reinforced by class agreement even at support 0.
  * first move: p = START, N_name(START -> r) = number of distinct mined
    propositions whose proof contains rule r at all (unigram count); same
    class back-off.

Search: best-first (priority queue) over the same state space, same dedup
keys, same depth bound / node budget / time budget as the uninformed runs in
euclid_search.prop_searches(). Priority = (-cumulative score, push order):
higher prior first, ties in FIFO push order. With an EMPTY prior (I.1 has no
earlier propositions) every score is 0 and tie-breaking alone orders the
queue, which reproduces the uninformed BFS expansion order exactly.

Chain motifs of length 3+ contain adjacent bigrams too; we use only 2-chain
(name-level) bigrams so a pair is counted once per proposition, never double
counted through longer chains.

Usage:
  python3 scripts/euclid_prior_search.py            # table for I.1-I.3
  python3 scripts/euclid_prior_search.py --prior-only

RESULT (2026-11-07, same budgets as euclid_search.prop_searches()):
motif prior alone: does NOT help (I.3 regresses — see above).
structural prior (bonus k for citing proposition_k, 0 for constructions):
I.1 identical (axioms only, bonus 0 everywhere). I.2 not found within
30 000 nodes / 600 s. I.3 still not found within 20 000 nodes — the bonus
is constant across a path, so it only reorders siblings and the
proposition_2' subtree still starves behind circle subtrees at equal
depth ordering.
motif + structural: I.3 IS found (depth 4, 16 948 nodes, ~45 s; a path
that applies proposition_2' twice — worse than uninformed BFS's 484
nodes at depth 3, but found). I.2 still not found. So: the structural
prior fixes the I.3 regression only in combination; nothing finds I.2.

"""

import argparse
import heapq
import math
import re
import time
from pathlib import Path

import importlib.util


def _load(name, path):
    spec = importlib.util.spec_from_file_location(name, Path(path))
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


_HERE = Path(__file__).resolve().parent
es = _load("euclid_search", _HERE / "euclid_search.py")
motifs = _load("euclid_step_motifs", _HERE / "euclid_step_motifs.py")

BETA = 0.5

# Structural prior (teacher's heuristic: the last thing proved is probably
# useful now): a citation proposition_k scores k (growing with k, so the
# most recent earlier proposition is highest), constructions score 0.
# Leakage: it reads only the NAMES of earlier propositions (k < n); the
# rule universe for target n is euclid_search's, whose citable rules are
# proposition_1 and proposition_2' only, so nothing >= n can ever be
# proposed. Mined proofs are not consulted.
STRUCT_W = 1.0


def structural_bonus(rule):
    m = re.match(r"proposition_(\d+)'*$", rule)
    return float(m.group(1)) if m else 0.0


# --- prior construction -------------------------------------------------------

def prop_number(theorem_name):
    m = re.match(r"proposition_(\d+)", theorem_name)
    if not m:
        return None
    return int(m.group(1))


def theorems_before(n, book_dir=motifs.DEFAULT_BOOK_DIR):
    """Theorems of propositions strictly < n. Primed variants share the
    proposition number: proposition_2' is INCLUDED for n=3 and excluded
    for n=2 (review fix, claude-9: the comment had this backwards)."""
    return [th for th in motifs.parse_book(book_dir)
            if (prop_number(th["name"]) or 999) < n]


def build_prior(n, book_dir=motifs.DEFAULT_BOOK_DIR):
    """Returns (prior, provenance) where prior is:

      name_bigram: {(p, r): count}        from chain motifs, name level
      class_bigram: {(pc, rc): count}     class level
      name_unigram: {r: count}            distinct propositions containing r
      class_unigram: {rc: count}

    All counts derive only from propositions < n. An empty dict set means
    "no earlier propositions" (n=1).
    """
    ths = theorems_before(n, book_dir)
    mined = motifs.mine(ths, min_support=1)
    name_bigram = {}
    class_bigram = {}
    for key, count in _bigram_counts(mined).items():
        level, p, r = key
        (name_bigram if level == "name" else class_bigram)[(p, r)] = count
    def _pairs(level):
        out = {}
        for key, info in mined["chain"][level].items():
            parts = tuple(k.strip() for k in key.split("->"))
            if len(parts) == 2:
                out[parts] = info["propositions"]
        return out

    # provenance for the leakage test: every 2-chain motif's supporting
    # propositions
    prov = {
        "name": _pairs("name"),
        "class": _pairs("class"),
        "theorems": [th["name"] for th in ths],
    }
    # unigrams from the same restricted theorems
    name_unigram = {}
    class_unigram = {}
    for th in ths:
        names = {s["name"] for s in th["steps"]}
        classes = {s["class"] for s in th["steps"]}
        for r in names:
            name_unigram[r] = name_unigram.get(r, 0) + 1
        for rc in classes:
            class_unigram[rc] = class_unigram.get(rc, 0) + 1
    prior = {
        "name_bigram": name_bigram,
        "class_bigram": class_bigram,
        "name_unigram": name_unigram,
        "class_unigram": class_unigram,
    }
    return prior, prov


def _bigram_counts(mined):
    """Support counts of 2-element chain motifs at both levels."""
    out = {}
    for level in ("name", "class"):
        for key, info in mined["chain"][level].items():
            parts = tuple(k.strip() for k in key.split("->"))
            if len(parts) == 2:
                out[(level,) + parts] = info["support"]
    return out


def move_score(prior, prev_rule, rule):
    """Documented formula. prev_rule None = START.

    Motif part: log1p(name bigram/unigram count) + BETA*log1p(class
    back-off). Structural part (when prior["structural"] is set):
    + STRUCT_W * structural_bonus(rule) — k for a citation proposition_k,
    0 for constructions; constant across the path, it only orders siblings.
    """
    cls = motifs.classify
    if prev_rule is None:
        n_name = prior["name_unigram"].get(rule, 0)
        n_cls = prior["class_unigram"].get(cls(rule), 0)
    else:
        n_name = prior["name_bigram"].get((prev_rule, rule), 0)
        n_cls = prior["class_bigram"].get(
            (cls(prev_rule), cls(rule)), 0)
    sc = math.log1p(n_name) + BETA * math.log1p(n_cls)
    if prior.get("structural"):
        sc += STRUCT_W * structural_bonus(rule)
    return sc


# --- best-first search ---------------------------------------------------------

def search_prior(hyps, rules, goal, max_depth, node_budget,
                 extra_la=None, time_budget=None, prior=None):
    """Same space/dedup/budgets as es.search, but pops the highest cumulative
    prior score first (ties: FIFO push order). Returns (found, path, stats).
    """
    es.reset_instruments()
    t0 = time.perf_counter()
    prior = prior or {}
    root = es.eng.State()
    for f, w in hyps:
        root.assume(f, w)
    for coeffs, const, op in (extra_la or []):
        root.la.add(coeffs, const, op)
    es.eng.close(root)
    if goal(root):
        return True, [], es._stats(0, [], 0.0)
    seen = {es.state_key(root)}
    counter = 0  # FIFO tie-break
    # heap items: (-cumulative_score, counter, state, path)
    heap = [(0.0, counter, root, [])]
    nodes = 0
    branch = []
    while heap:
        if nodes >= node_budget:
            break
        if time_budget and time.perf_counter() - t0 > time_budget:
            break
        neg_total, _, s, path = heapq.heappop(heap)
        nodes += 1
        if len(path) >= max_depth:
            continue
        moves = es.legal_moves(s, rules)
        branch.append(len(moves))
        prev_rule = path[-1][0] if path else None
        for rname, args in moves:
            s2 = es.clone_state(s)
            fresh_before = {x for f in s2.facts for x in f[1:] if es._is_fresh(x)}
            try:
                es.eng.apply_step(s2, len(path) + 1, rname, args, {})
            except es.eng.MissingPremise:
                continue
            k = es.state_key(s2)
            if k in seen:
                continue
            seen.add(k)
            new = ({x for f in s2.facts for x in f[1:] if es._is_fresh(x)}
                   - fresh_before)
            step = (rname, args, es._bindings(rname, new))
            total = -neg_total + move_score(prior, prev_rule, rname)
            counter += 1
            heapq.heappush(heap, (-total, counter, s2, path + [step]))
            if goal(s2) and es.store_consistent(s2.la):
                return True, path + [step], es._stats(
                    nodes, branch + [len(moves)],
                    time.perf_counter() - t0)
    return False, None, es._stats(nodes, branch, time.perf_counter() - t0)


def structural_prior():
    """Structural-only prior: no mined counts, just the recency bonus."""
    return {"name_bigram": {}, "class_bigram": {},
            "name_unigram": {}, "class_unigram": {}, "structural": True}


# --- comparison table -----------------------------------------------------------

PROP_N = {"I.1": 1, "I.2": 2, "I.3": 3}


def run_table(props=("I.1", "I.2", "I.3"),
              variants=("uninformed", "motif", "structural", "motif+struct")):
    searches = es.prop_searches()
    rows = []
    for name in props:
        kw = searches[name]
        n = PROP_N[name]
        mined, _ = build_prior(n)
        for var in variants:
            if var == "uninformed":
                found, path, st = es.search(**kw)
            elif var == "structural":
                found, path, st = search_prior(prior=structural_prior(), **kw)
            elif var == "motif":
                found, path, st = search_prior(prior=mined, **kw)
            else:  # motif+struct
                comb = dict(mined)
                comb["structural"] = True
                found, path, st = search_prior(prior=comb, **kw)
            rows.append((name, var, found, path, st))
            print(f"--- {name} [{var}]: {'FOUND' if found else 'not found'}")
            print(f"    depth={len(path) if found else '-'} nodes={st['nodes']} "
                  f"wall={st['wall_s']:.2f}s entails={st['entails_s']:.2f}s")
            if found:
                for i, (r, a, b) in enumerate(path, 1):
                    print(f"    {i}. {r} {a} {b}")
                es.verify_path(kw["hyps"], path, kw["goal"], kw.get("extra_la"))
                print("    replay: OK")
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--prior-only", action="store_true",
                    help="just print the priors and their provenance")
    args = ap.parse_args()
    if args.prior_only:
        for n in (1, 2, 3):
            prior, prov = build_prior(n)
            print(f"=== prior for proposition {n} "
                  f"(theorems: {prov['theorems']})")
            for k in ("name_unigram", "name_bigram", "class_bigram"):
                print(f"  {k}: {prior[k]}")
        return
    run_table()


if __name__ == "__main__":
    main()
