#!/usr/bin/env python3
"""Build, train, and evaluate the DESIGN-3 E2 ZERO policy dataset."""
from __future__ import annotations

import argparse
import gzip
import hashlib
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
    payload = {"examples": examples,
               "record_counts": {f"{s}:{h}": n for (s, h), n in counts.items()},
               "vocabulary_version": features.VOCABULARY.vocabulary_version}
    Path(output).parent.mkdir(parents=True, exist_ok=True)
    with gzip.open(output, "wb", compresslevel=1) as stream:
        pickle.dump(payload, stream, protocol=5)
    summary = {"records": sum(counts.values()), "examples": len(examples),
               "train_examples": sum(x["split"] == "train" for x in examples),
               "heldout_examples": sum(x["split"] == "heldout" for x in examples),
               "record_counts": payload["record_counts"]}
    Path(str(output) + ".json").write_text(json.dumps(summary, indent=2) + "\n")
    print(json.dumps(summary, sort_keys=True))


def _batch(items):
    import jax.numpy as jnp
    arrays = models.batch_features([item["features"] for item in items])
    width = arrays["candidate_mask"].shape[1]
    target = np.zeros((len(items), width), dtype=np.float32)
    for row, item in enumerate(items):
        target[row, :len(item["target"])] = item["target"]
    return arrays, jnp.asarray(target)


def train(dataset, out, duration, seed, batch_size, learning_rate):
    from flax import nnx
    import jax
    import jax.numpy as jnp
    import optax
    with gzip.open(dataset, "rb") as stream:
        examples = [x for x in pickle.load(stream)["examples"]
                    if x["split"] == "train"]
    rng = random.Random(seed)
    model = models.CandidateModel(seed=seed)
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
        loss = float(step(model, optimizer, *_batch(items)))
        step_number += 1
        if step_number == 1 or step_number % 25 == 0:
            row = {"step": step_number, "loss": loss, "elapsed": duration - max(0, deadline-time.monotonic())}
            curve.append(row)
            with (out / "train.jsonl").open("a") as log:
                log.write(json.dumps(row) + "\n")
    models.save_plain_arrays(model, out / "model")
    (out / "summary.json").write_text(json.dumps(
        {"steps": step_number, "curve": curve, "value_weight": 0.0,
         "seed": seed, "batch_size": batch_size,
         "learning_rate": learning_rate}, indent=2) + "\n")


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
    result={"heldout":{},"book":{},"leakage":{}}
    train_keys={x["state_key"] for x in payload["examples"] if x["split"]=="train"}
    test_keys={x["state_key"] for x in held}
    result["leakage"]={"heldout_in_train":len(train_keys & test_keys)}
    for name,model in models_to_test.items():
        top1=top5=0
        for item in held:
            batch=models.batch_features([item["features"]]); logits,_=model(batch)
            order=np.argsort(-np.asarray(logits[0,:len(item["target"])]),kind="stable")
            positive=set(np.flatnonzero(item["target"])); top1+=bool(set(order[:1])&positive); top5+=bool(set(order[:5])&positive)
        result["heldout"][name]={"positions":len(held),"top1":top1/len(held),"top5":top5/len(held)}
        rows={}
        for label,state,legal,wins in book_positions():
            ranking=ranks(model,state,legal,None); rows[label]={"candidates":len(legal),"rank":rank_tuple(ranking,legal,wins),"state_key":state_key(state)}
        result["book"][name]=rows
    book_keys={row["state_key"] for rows in result["book"].values() for row in rows.values()}
    result["leakage"]["book_in_train"]=len(train_keys & book_keys)
    Path(output).write_text(json.dumps(result,indent=2)+"\n"); print(json.dumps(result))


def main():
    p=argparse.ArgumentParser(); sub=p.add_subparsers(dest="cmd",required=True)
    b=sub.add_parser("build"); b.add_argument("--filtered",required=True); b.add_argument("--output",required=True)
    t=sub.add_parser("train"); t.add_argument("--dataset",required=True); t.add_argument("--out",required=True); t.add_argument("--duration",type=int,default=2400); t.add_argument("--seed",type=int,default=2026111202); t.add_argument("--batch-size",type=int,default=16); t.add_argument("--learning-rate",type=float,default=1e-3)
    e=sub.add_parser("eval"); e.add_argument("--dataset",required=True); e.add_argument("--trained",required=True); e.add_argument("--legacy",required=True); e.add_argument("--output",required=True); e.add_argument("--seed",type=int,default=2026111202)
    a=p.parse_args()
    if a.cmd=="build": build_dataset(a.filtered,a.output)
    elif a.cmd=="train": train(a.dataset,a.out,a.duration,a.seed,a.batch_size,a.learning_rate)
    else: evaluate(a.dataset,a.trained,a.legacy,a.output,a.seed)

if __name__=="__main__": main()
