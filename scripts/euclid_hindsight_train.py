#!/usr/bin/env python3
"""Build, train, and evaluate the DESIGN-3 E2 ZERO policy dataset."""
from __future__ import annotations

import argparse
import glob
import gzip
import hashlib
import itertools
import json
import pickle
from pathlib import Path
import random
import time

import numpy as np
import pyspiel

import euclid_candidate_features as features
import euclid_candidate_model as models
import euclid_hindsight as hindsight
import euclid_spiel


def candidates(state):
    generated = euclid_spiel.enumerate_actions(
        state._proposition, state.engine_state, state.initial_kinds,
        len(state.history_moves), canonical_names=state._canonical_names(),
        validate=False)
    objects = set(state._object_order())
    # The full engine generator can ground stated-input rules with RESERVED
    # forward names.  Those names deliberately are not objects yet and hence
    # have no candidate-object embedding.  The scorer's candidate space is
    # the premise-certified, feature-encodable subset.
    return [action for action in generated
            if all(not isinstance(arg, str) or arg in objects
                   for arg in action[1])]


def action_signature(action):
    return action[0], tuple(action[1])


def split_for(proposition, seed):
    digest = hashlib.sha256(f"{proposition}:{seed}".encode()).digest()
    return "heldout" if int.from_bytes(digest[:8], "big") % 10 == 0 else "train"


def state_key(state):
    return hashlib.sha256(repr(state.logical_state_key()).encode()).hexdigest()


def _state(proposition):
    game = pyspiel.load_game(f"euclid_book1(proposition={proposition},move_cap=20)")
    return game.new_initial_state()


def make_state_disjoint(examples):
    """Move held-out copies of training states into the training split."""
    train_keys = {x["state_key"] for x in examples if x["split"] == "train"}
    shared_keys = train_keys & {
        x["state_key"] for x in examples if x["split"] == "heldout"
    }
    moved = 0
    for example in examples:
        if example["split"] == "heldout" and example["state_key"] in shared_keys:
            example["split"] = "train"
            moved += 1
    return len(shared_keys), moved


def goal_atoms(goal):
    if goal and isinstance(goal[0], (tuple, list)):
        return tuple(tuple(atom) for atom in goal)
    return (tuple(goal),)


def _atom_variants(atom):
    """Orderings of an atom that state the same fact."""
    head = atom[0]
    if head == "_eq_len" and len(atom) == 5:
        _, p, q, r, t = atom
        out = []
        for s1, s2 in (((p, q), (r, t)), ((r, t), (p, q))):
            for a in (s1, s1[::-1]):
                for b in (s2, s2[::-1]):
                    out.append((head, *a, *b))
        return out
    if head == "between" and len(atom) == 4:
        return [atom, (head, atom[3], atom[2], atom[1])]
    if head == "_eq_ang" and len(atom) == 7:
        # angle pqr = angle stu; each angle reads the same backwards, and
        # the two sides of the equation can swap (review fix, claude-9).
        _, p, q, r, s, t, u = atom
        out = []
        for a1, a2 in (((p, q, r), (s, t, u)), ((s, t, u), (p, q, r))):
            for x in (a1, a1[::-1]):
                for y in (a2, a2[::-1]):
                    out.append((head, *x, *y))
        return out
    flip = {"_gt_len": "_lt_len", "_lt_len": "_gt_len",
            "_gt_ang": "_lt_ang", "_lt_ang": "_gt_ang"}
    if head in ("_gt_len", "_lt_len") and len(atom) == 5:
        _, p, q, r, t = atom
        out = []
        for h, s1, s2 in ((head, (p, q), (r, t)), (flip[head], (r, t), (p, q))):
            for x in (s1, s1[::-1]):
                for y in (s2, s2[::-1]):
                    out.append((h, *x, *y))
        return out
    if head in ("_gt_ang", "_lt_ang") and len(atom) == 7:
        _, p, q, r, s, t, u = atom
        out = []
        for h, a1, a2 in ((head, (p, q, r), (s, t, u)), (flip[head], (s, t, u), (p, q, r))):
            for x in (a1, a1[::-1]):
                for y in (a2, a2[::-1]):
                    out.append((h, *x, *y))
        return out
    return [atom]


def _goal_shape(atoms, rename):
    """A goal as (classes of equal segments, other atoms), points renamed.

    Equal-length atoms are closed under transitivity, so |ab|=|bc|, |ac|=|bc|
    and |ca|=|ab|, |cb|=|ab| (I.1) have the same shape."""
    parent = {}
    def find(x):
        parent.setdefault(x, x)
        while parent[x] != x:
            x = parent[x]
        return x
    others = set()
    for atom in atoms:
        if atom[0] == "_eq_len" and len(atom) == 5:
            p, q, r, t = (rename(x) for x in atom[1:])
            s1, s2 = frozenset((p, q)), frozenset((r, t))
            parent[find(s1)] = find(s2)
        else:
            variants = [tuple(rename(x) if i else x for i, x in enumerate(v))
                        for v in _atom_variants(atom)]
            others.add(min(variants))
    classes = {}
    for seg in list(parent):
        classes.setdefault(find(seg), set()).add(seg)
    return (frozenset(frozenset(c) for c in classes.values() if len(c) > 1),
            frozenset(others))


def _points(atoms):
    return {x for atom in atoms for x in atom[1:]}


def book_goal_match(record):
    """Whether a record's goal states a Book goal of I.1 through I.25.

    Review fix (claude-9): the first version compared only with the record's
    own proposition, kept given points fixed and matched atom by atom, so
    I.1's goal reached by random play from I.2's givens (stated as
    |ab|=|bc|, |ac|=|bc|) was not excluded.  Now every Book goal is tried;
    Book given points map injectively to the record's own given points and
    witnesses to made (non-given) objects; equal lengths compare as classes
    of equal segments.  Returns (proposition, mapping) or None.
    """
    actual = goal_atoms(record["goal"])
    # Arithmetic closure also emits facts with structured terms (for example
    # ``sublen (len a b) a c``).  Book goals are relations between named
    # geometric objects, so such a record cannot match and must not enter the
    # point-renaming search.
    if any(not isinstance(argument, str)
           for atom in actual for argument in atom[1:]):
        return None
    own_given = {obj["name"] for obj in
                 features.play.tr.objects_from_hyps(record["proposition"])}
    actual_points = sorted(_points(actual))
    target = _goal_shape(actual, lambda x: x)
    for book in range(1, 26):
        expected = tuple(tuple(atom) for atom in features.play.GOALS[book])
        book_points = sorted(_points(expected))
        if len(book_points) != len(actual_points):
            continue
        book_given = {obj["name"] for obj in
                      features.play.tr.objects_from_hyps(book)}
        for image in itertools.permutations(actual_points):
            mapping = dict(zip(book_points, image))
            if any((k in book_given) != (v in own_given) for k, v in mapping.items()):
                continue
            if _goal_shape(expected, mapping.get) == target:
                return book, mapping
    return None


def _book_state_keys():
    states = [_state(1), _state(2), _state(3)]
    states.extend(row[1] for row in book_positions())
    return {state_key(state) for state in states}


def _record_notation(record):
    needed = set(record["needed_move_indices"])
    return {
        "proposition": record["proposition"],
        "seed": record["run_seed"],
        "goal": record["goal"],
        "moves": [move["notation"] for i, move in enumerate(record["moves"])
                  if i in needed],
    }


def build_mixed_dataset(single_filtered, conjunctive_inputs, output,
                        lookahead=False):
    examples, counts, seen_records = [], {}, set()
    excluded_goals, excluded_states = [], 0
    paths = [(single_filtered, "single")]
    paths += [(path, "conjunctive")
              for path in sorted(glob.glob(conjunctive_inputs or ""))]
    forbidden_states = _book_state_keys()
    for path, expected_kind in paths:
        with open(path) as stream:
            for line in stream:
                record = json.loads(line)
                kind = "conjunctive" if record.get("conjunctive") else "single"
                if kind != expected_kind:
                    continue
                identity = (record["proposition"], record["canonical_key"])
                if identity in seen_records:
                    continue
                seen_records.add(identity)
                atoms = goal_atoms(record["goal"])
                if record.get("trivial_reasons") or any(
                        atom[0] in ("pos_len", "neq") for atom in atoms):
                    continue
                match = book_goal_match(record)
                if match is not None:
                    excluded_goals.append(_record_notation(record))
                    continue
                split = split_for(record["proposition"], record["run_seed"])
                moves = [(m["tuple"][0], tuple(m["tuple"][1]),
                          tuple(sorted(m["tuple"][2].items())))
                         for m in record["moves"]]
                needed = list(record["needed_move_indices"])
                state = _state(record["proposition"])
                record_examples = 0
                for offset, move_index in enumerate(needed):
                    legal = candidates(state)
                    remaining = {action_signature(moves[i])
                                 for i in needed[offset:]}
                    positive = [i for i, action in enumerate(legal)
                                if action_signature(action) in remaining]
                    key = state_key(state)
                    if positive and key not in forbidden_states:
                        target = np.zeros(len(legal), dtype=np.float32)
                        target[positive] = 1.0 / len(positive)
                        examples.append({
                            "features": features.extract(
                                state, legal, goal=record["goal"],
                                lookahead=lookahead).arrays,
                            "target": target, "split": split, "kind": kind,
                            "proposition": record["proposition"],
                            "seed": record["run_seed"], "goal": record["goal"],
                            "state_key": key,
                        })
                        record_examples += 1
                    elif positive:
                        excluded_states += 1
                    state.apply_action_tuple(moves[move_index])
                if record_examples:
                    counts[(split, kind)] = counts.get((split, kind), 0) + 1
    shared_key_count, moved_examples = make_state_disjoint(examples)
    payload = {"examples": examples,
               "record_counts": {f"{s}:{k}": n for (s, k), n in counts.items()},
               "vocabulary_version": features.VOCABULARY.vocabulary_version,
               "lookahead": lookahead}
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    with gzip.open(output, "wb", compresslevel=1) as stream:
        pickle.dump(payload, stream, protocol=5)
    summary = {
        "records": sum(counts.values()), "examples": len(examples),
        "train_examples": sum(x["split"] == "train" for x in examples),
        "heldout_examples": sum(x["split"] == "heldout" for x in examples),
        "examples_by_kind": {
            kind: sum(x["kind"] == kind for x in examples)
            for kind in ("single", "conjunctive")},
        "heldout_by_kind": {
            kind: sum(x["kind"] == kind and x["split"] == "heldout"
                      for x in examples)
            for kind in ("single", "conjunctive")},
        "shared_state_keys_moved_to_train": shared_key_count,
        "heldout_examples_moved_to_train": moved_examples,
        "book_goal_records_excluded": len(excluded_goals),
        "book_goal_exclusions": excluded_goals,
        "book_state_examples_excluded": excluded_states,
        "record_counts": payload["record_counts"], "lookahead": lookahead,
    }
    Path(str(output) + ".json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary, sort_keys=True))


def build_dataset(filtered, output):
    examples, counts, seen_records = [], {}, set()
    with open(filtered) as stream:
        for line in stream:
            record = json.loads(line)
            identity = (record["proposition"], record["canonical_key"])
            if identity in seen_records:
                continue
            seen_records.add(identity)
            if record["trivial_reasons"] or record["goal"][0] in ("pos_len", "neq"):
                continue
            split = split_for(record["proposition"], record["run_seed"])
            counts[(split, record["goal"][0])] = counts.get(
                (split, record["goal"][0]), 0) + 1
            moves = [(m["tuple"][0], tuple(m["tuple"][1]),
                      tuple(sorted(m["tuple"][2].items()))) for m in record["moves"]]
            needed = list(record["needed_move_indices"])
            state = _state(record["proposition"])
            for offset, move_index in enumerate(needed):
                legal = candidates(state)
                remaining = {action_signature(moves[i]) for i in needed[offset:]}
                positive = [i for i, action in enumerate(legal)
                            if action_signature(action) in remaining]
                if positive:
                    target = np.zeros(len(legal), dtype=np.float32)
                    target[positive] = 1.0 / len(positive)
                    examples.append({
                        "features": features.extract(
                            state, legal, goal=tuple(record["goal"])).arrays,
                        "target": target, "split": split,
                        "proposition": record["proposition"],
                        "seed": record["run_seed"], "goal": record["goal"],
                        "state_key": state_key(state),
                    })
                state.apply_action_tuple(moves[move_index])
    # A split by run seed alone is not state-disjoint: independent runs can
    # reach the same logical position.  Training owns every shared state so
    # that evaluation contains no position the model trained on.
    shared_key_count, moved_examples = make_state_disjoint(examples)
    payload = {"examples": examples,
               "record_counts": {f"{s}:{h}": n for (s, h), n in counts.items()},
               "vocabulary_version": features.VOCABULARY.vocabulary_version}
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    with gzip.open(output, "wb", compresslevel=1) as stream:
        pickle.dump(payload, stream, protocol=5)
    summary = {"records": sum(counts.values()), "examples": len(examples),
               "train_examples": sum(x["split"] == "train" for x in examples),
               "heldout_examples": sum(x["split"] == "heldout" for x in examples),
               "shared_state_keys_moved_to_train": shared_key_count,
               "heldout_examples_moved_to_train": moved_examples,
               "record_counts": payload["record_counts"]}
    Path(str(output) + ".json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary, sort_keys=True))


def _batch(items, shapes=None):
    import jax.numpy as jnp
    arrays = models.batch_features([item["features"] for item in items],
                                   bucket=shapes is None, shapes=shapes)
    width = arrays["candidate_mask"].shape[1]
    target = np.zeros((len(items), width), dtype=np.float32)
    for row, item in enumerate(items):
        target[row, :len(item["target"])] = item["target"]
    return arrays, jnp.asarray(target)


def train(dataset, out, duration, seed, batch_size, learning_rate,
          lookahead=False):
    from flax import nnx
    import jax
    import jax.numpy as jnp
    import optax
    with gzip.open(dataset, "rb") as stream:
        examples = [x for x in pickle.load(stream)["examples"]
                    if x["split"] == "train"]
    if not lookahead:
        for example in examples:
            example["features"].pop("candidate_join_count", None)
    rng = random.Random(seed)
    # One padded shape for the whole training set: the jitted step compiles once.
    shapes = models.max_shapes([x["features"] for x in examples])
    model = models.CandidateModel(seed=seed, lookahead=lookahead)
    optimizer = nnx.Optimizer(model, optax.adam(learning_rate), wrt=nnx.Param)

    def loss_fn(network, arrays, target):
        logits, _value = network(arrays)
        logits = jnp.where(arrays["candidate_mask"], logits, -1e30)
        return -jnp.mean(jnp.sum(target * jax.nn.log_softmax(logits, axis=1), axis=1))

    @nnx.jit
    def step(network, opt, arrays, target):
        loss, gradients = nnx.value_and_grad(loss_fn)(network, arrays, target)
        opt.update(network, gradients)
        return loss

    out = Path(out); out.mkdir(parents=True, exist_ok=True)
    deadline, step_number, curve = time.monotonic() + duration, 0, []
    while time.monotonic() < deadline:
        items = rng.sample(examples, min(batch_size, len(examples)))
        loss = float(step(model, optimizer, *_batch(items, shapes)))
        step_number += 1
        if step_number % 250 == 0:  # periodic snapshot: a killed run keeps its latest model
            models.save_plain_arrays(model, out / f"model-step-{step_number:06d}")
        if step_number == 1 or step_number % 25 == 0:
            row = {"step": step_number, "loss": loss, "elapsed": duration - max(0, deadline-time.monotonic())}
            curve.append(row)
            with (out / "train.jsonl").open("a") as log:
                log.write(json.dumps(row) + "\n")
    models.save_plain_arrays(model, out / "model")
    (out / "summary.json").write_text(json.dumps(
        {"steps": step_number, "curve": curve, "value_weight": 0.0,
         "seed": seed, "batch_size": batch_size,
         "learning_rate": learning_rate, "lookahead": lookahead}, indent=2) + "\n")


def load_legacy(model, source):
    """Evaluation-only prefix migration; normal snapshot loader still refuses."""
    from flax import nnx
    import jax
    import jax.numpy as jnp
    manifest = json.loads((Path(source) / "manifest.json").read_text())
    state = nnx.state(model, nnx.Param)
    leaves, tree = jax.tree_util.tree_flatten(state)
    loaded = []
    for target, entry in zip(leaves, manifest):
        old = np.load(Path(source) / entry["file"])
        if old.shape == target.shape:
            loaded.append(jnp.asarray(old)); continue
        if old.ndim == target.ndim and old.shape[1:] == target.shape[1:] and old.shape[0] < target.shape[0]:
            new = np.asarray(target).copy(); new[:old.shape[0]] = old
            loaded.append(jnp.asarray(new)); continue
        raise ValueError((old.shape, target.shape))
    nnx.update(model, jax.tree_util.tree_unflatten(tree, loaded))


def ranks(model, state, legal, goal):
    from flax import nnx
    batch = models.batch_features([features.extract(state, legal, goal=goal).arrays])
    logits, _ = nnx.jit(lambda net, x: net(x))(model, batch)
    values = np.asarray(logits[0, :len(legal)])
    order = np.argsort(-values, kind="stable")
    result = np.empty(len(order), dtype=int); result[order] = np.arange(1, len(order)+1)
    return result


def rank_tuple(ranking, legal, indexes):
    best = min(indexes, key=lambda i: ranking[i])
    rule = legal[best][0]
    rule_order = sorted({a[0] for a in legal}, key=lambda r: min(ranking[i] for i,a in enumerate(legal) if a[0]==r))
    within = sorted((i for i,a in enumerate(legal) if a[0]==rule), key=lambda i: ranking[i]).index(best)+1
    return int(ranking[best]), rule_order.index(rule)+1, within


def probability_and_rank(model, state, legal, indexes, goal=None):
    """Return the best requested action's stable rank and softmax mass."""
    from flax import nnx
    batch = models.batch_features([features.extract(state, legal, goal=goal).arrays])
    logits, _ = nnx.jit(lambda net, x: net(x))(model, batch)
    values = np.asarray(logits[0, :len(legal)], dtype=np.float64)
    shifted = values - values.max()
    probabilities = np.exp(shifted) / np.exp(shifted).sum()
    order = np.argsort(-values, kind="stable")
    ranking = np.empty(len(order), dtype=int)
    ranking[order] = np.arange(1, len(order) + 1)
    best = min(indexes, key=lambda i: ranking[i])
    return {"rank": int(ranking[best]), "probability": float(probabilities[best]),
            "candidates": len(legal), "action": legal[best]}


def i1_probe_positions():
    after_one = _state(1)
    after_one.apply_action_tuple(
        ("circle_from_points", ("b", "a"), (("C", "C_1"),)))
    legal_one = candidates(after_one)
    second = [i for i, action in enumerate(legal_one)
              if action[0] == "circle_from_points"
              and action[1] == ("a", "b")]
    after_two = after_one.clone()
    after_two.apply_action_tuple(
        ("circle_from_points", ("a", "b"), (("C", "C_2"),)))
    legal_two = candidates(after_two)
    intersection = [i for i, action in enumerate(legal_two)
                    if action[0] == "intersection_circles"
                    and action[1] == ("C_1", "C_2")]
    return [("second-circle", after_one, legal_one, second),
            ("intersection", after_two, legal_two, intersection)]


def book_positions():
    i1 = _state(1)
    i1.apply_action_tuple(("circle_from_points", ("a","b"), (("C","C_1"),)))
    i1.apply_action_tuple(("circle_from_points", ("b","a"), (("C","C_2"),)))
    l1 = candidates(i1); w1=[i for i,a in enumerate(l1) if a[0]=="intersection_circles" and a[1]==("C_1","C_2")]
    i3 = _state(3)
    i3.apply_action_tuple(("proposition_2'", ("a","c0","c1","C"), (("d","d_1"),)))
    i3.apply_action_tuple(("circle_from_points", ("a","d_1"), (("C","C_2"),)))
    l3=candidates(i3); w3=[i for i,a in enumerate(l3) if a[0]=="intersection_circle_line_between_points" and a[1]==("C_2","AB","a","b")]
    import euclid_candidate_checks_8c as c8
    prefix=Path(__file__).resolve().parents[1]/"holes/excursions/E-euclid-tutor-data/spiel/rc13/stage-2/prefix.json"
    i2=c8.game(prefix).new_initial_state(); l2=c8.candidates(i2)
    return [("I.1",i1,l1,w1),("I.3",i3,l3,w3),("I.2-stage2",i2,l2,[24,370,371,372,373,374,375])]


def evaluate(dataset, trained, legacy, output, seed):
    with gzip.open(dataset,"rb") as stream: payload=pickle.load(stream)
    held=[x for x in payload["examples"] if x["split"]=="heldout"]
    models_to_test={}
    fresh=models.CandidateModel(seed=seed); models_to_test["generation0"]=fresh
    trained_model=models.CandidateModel(seed=seed); models.load_plain_arrays(trained_model,trained); models_to_test["trained"]=trained_model
    old=models.CandidateModel(seed=seed); load_legacy(old,legacy); models_to_test["packet8b"]=old
    result={"heldout":{},"book":{},"probe":{},"leakage":{}}
    train_keys={x["state_key"] for x in payload["examples"] if x["split"]=="train"}
    test_keys={x["state_key"] for x in held}
    result["leakage"]={"heldout_in_train":len(train_keys & test_keys)}
    for name,model in models_to_test.items():
        heldout_rows = {}
        for kind in (None, "single", "conjunctive"):
            selected = held if kind is None else [x for x in held if x.get("kind") == kind]
            top1=top5=0
            for item in selected:
                batch=models.batch_features([item["features"]]); logits,_=model(batch)
                order=np.argsort(-np.asarray(logits[0,:len(item["target"])]),kind="stable")
                positive=set(np.flatnonzero(item["target"])); top1+=bool(set(order[:1])&positive); top5+=bool(set(order[:5])&positive)
            heldout_rows[kind or "all"]={"positions":len(selected),
                "top1":top1/len(selected),"top5":top5/len(selected)}
        result["heldout"][name]=heldout_rows
        rows={}
        for label,state,legal,wins in book_positions():
            ranking=ranks(model,state,legal,None); rows[label]={"candidates":len(legal),"rank":rank_tuple(ranking,legal,wins),"state_key":state_key(state)}
        result["book"][name]=rows
        result["probe"][name] = {
            label: probability_and_rank(model, state, legal, wins, goal=None)
            for label, state, legal, wins in i1_probe_positions()
        }
    book_keys={row["state_key"] for rows in result["book"].values() for row in rows.values()}
    result["leakage"]["book_in_train"]=len(train_keys & book_keys)
    Path(output).write_text(json.dumps(result,indent=2)+"\n"); print(json.dumps(result))


def main():
    p=argparse.ArgumentParser(); sub=p.add_subparsers(dest="cmd",required=True)
    b=sub.add_parser("build"); b.add_argument("--filtered",required=True); b.add_argument("--output",required=True)
    bm=sub.add_parser("build-mixed"); bm.add_argument("--single-filtered",required=True); bm.add_argument("--conjunctive-inputs",required=True); bm.add_argument("--output",required=True); bm.add_argument("--lookahead",action="store_true")
    t=sub.add_parser("train"); t.add_argument("--dataset",required=True); t.add_argument("--out",required=True); t.add_argument("--duration",type=int,default=2400); t.add_argument("--seed",type=int,default=2026111202); t.add_argument("--batch-size",type=int,default=16); t.add_argument("--learning-rate",type=float,default=1e-3); t.add_argument("--lookahead",action="store_true")
    e=sub.add_parser("eval"); e.add_argument("--dataset",required=True); e.add_argument("--trained",required=True); e.add_argument("--legacy",required=True); e.add_argument("--output",required=True); e.add_argument("--seed",type=int,default=2026111202)
    a=p.parse_args()
    if a.cmd=="build": build_dataset(a.filtered,a.output)
    elif a.cmd=="build-mixed": build_mixed_dataset(a.single_filtered,a.conjunctive_inputs,a.output,a.lookahead)
    elif a.cmd=="train": train(a.dataset,a.out,a.duration,a.seed,a.batch_size,a.learning_rate,a.lookahead)
    else: evaluate(a.dataset,a.trained,a.legacy,a.output,a.seed)

if __name__=="__main__": main()
