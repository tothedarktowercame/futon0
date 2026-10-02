#!/usr/bin/env python3
"""Build the deterministic R-node cue vocabulary consumed by session-mode."""

import hashlib
import json
from collections import defaultdict
from pathlib import Path

import edn_format
from edn_format import Keyword as K


HERE = Path(__file__).resolve().parent
STAGES = Path("/home/joe/code/p4ng/empirics-futon/control-stages.edn")
OUTPUT = HERE / "rnode-vocabulary.json"

GENERIC_CUES = {
    "would", "work", "will", "i think", "we could", "build", "design",
    "until", "if we", "rather ... than", "level", "enough",
}
EXCLUDED_NODES = {"R9", "R12", "R3a", "R19", "R8"}
EXCLUDED_NODE_REASON = "detections close to noise; leave to later 象 step"


def normalized(cue):
    return " ".join(str(cue).strip().lower().split())


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def tree_catalogue(tree):
    catalogue, cues = {}, defaultdict(set)

    def walk(node):
        leaf = node.get(K("leaf"))
        if leaf:
            leaf = str(leaf)
            catalogue[leaf] = str(node[K("label")])
            cues[leaf].update(normalized(c) for c in node.get(K("cues"), []))
        for child in node.get(K("children"), []):
            walk(child)

    for branch in tree[K("branches")]:
        walk(branch)
    return catalogue, cues


def build(tree_path, elaboration_path, machine_path, stages_path):
    paths = [Path(p) for p in (tree_path, elaboration_path, machine_path, stages_path)]
    tree = edn_format.loads(paths[0].read_text())
    catalogue, cues = tree_catalogue(tree)

    elaboration = json.loads(paths[1].read_text())
    for item in elaboration["kept-cues"]:
        node = item["node"]
        if node in catalogue:
            cues[node].add(normalized(item["cue"]))

    machine = edn_format.loads(paths[2].read_text())
    for node, node_cues in machine[K("cues")].items():
        node = str(node)
        if node in catalogue:
            cues[node].update(normalized(c) for c in node_cues)

    stages_doc = edn_format.loads(paths[3].read_text())
    stage_rows = {str(row[K("node")]): row for row in stages_doc[K("nodes")]}

    cue_nodes = defaultdict(set)
    for node, node_cues in cues.items():
        for cue in node_cues:
            cue_nodes[cue].add(node)
    collisions = {cue: sorted(nodes) for cue, nodes in cue_nodes.items()
                  if len(nodes) >= 2}

    nodes = []
    for node in sorted(catalogue):
        if node in EXCLUDED_NODES:
            continue
        row = stage_rows[node]
        stage = "assurance" if row.get(K("band")) == K("assurance") \
            else str(row[K("stage")]).lower()
        kept = sorted(cue for cue in cues[node]
                      if cue not in GENERIC_CUES and cue not in collisions)
        nodes.append({"id": node, "label": catalogue[node],
                      "stage": stage, "cues": kept})

    return {
        "version": 1,
        "sources": {path.name: sha256(path) for path in paths},
        "nodes": nodes,
        "excluded": {
            "generic": sorted(GENERIC_CUES),
            "collisions": [
                {"cue": cue, "nodes": nodes}
                for cue, nodes in sorted(collisions.items())
            ],
            "nodes": [
                {"id": node, "reason": EXCLUDED_NODE_REASON}
                for node in sorted(EXCLUDED_NODES)
            ],
        },
    }


def main():
    doc = build(HERE / "rnode-tree.edn", HERE / "ELAB-collated.json",
                HERE / "machine-cues.edn", STAGES)
    OUTPUT.write_text(json.dumps(doc, ensure_ascii=False, indent=2) + "\n")
    print(f"wrote {OUTPUT}: {len(doc['nodes'])} nodes, "
          f"{sum(len(node['cues']) for node in doc['nodes'])} cues")


if __name__ == "__main__":
    main()
