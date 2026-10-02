import json
from pathlib import Path

import edn_format

import build_vocabulary as vocabulary


HERE = Path(__file__).resolve().parent
STAGES = Path("/home/joe/code/p4ng/empirics-futon/control-stages.edn")


def production():
    return vocabulary.build(HERE / "rnode-tree.edn", HERE / "ELAB-collated.json",
                            HERE / "machine-cues.edn", STAGES)


def test_no_excluded_cue_survives():
    doc = production()
    surviving = {cue for node in doc["nodes"] for cue in node["cues"]}
    assert not surviving.intersection(doc["excluded"]["generic"])
    assert not surviving.intersection(row["cue"] for row in doc["excluded"]["collisions"])
    assert not {node["id"] for node in doc["nodes"]}.intersection(
        row["id"] for row in doc["excluded"]["nodes"])


def test_no_cue_is_under_two_nodes():
    owner = {}
    for node in production()["nodes"]:
        for cue in node["cues"]:
            assert cue not in owner, (cue, owner[cue], node["id"])
            owner[cue] = node["id"]


def test_every_output_node_is_a_catalogue_node():
    tree = edn_format.loads((HERE / "rnode-tree.edn").read_text())
    catalogue, _ = vocabulary.tree_catalogue(tree)
    assert {node["id"] for node in production()["nodes"]} <= set(catalogue)


def test_three_cue_fixture_by_hand(tmp_path):
    tree = tmp_path / "rnode-tree.edn"
    elaboration = tmp_path / "ELAB-collated.json"
    machine = tmp_path / "machine-cues.edn"
    stages = tmp_path / "control-stages.edn"
    tree.write_text('{:branches [{:leaf "A" :label "Alpha" :cues ["one" "shared"]}\n'
                    '            {:leaf "B" :label "Beta" :cues ["shared"]}]}')
    elaboration.write_text(json.dumps({"kept-cues": [{"node": "A", "cue": "two"}]}))
    machine.write_text('{:cues {}}')
    stages.write_text('{:nodes [{:node "A" :stage "SELECT" :band :loop}\n'
                      '         {:node "B" :stage "ACT" :band :assurance}]}')

    doc = vocabulary.build(tree, elaboration, machine, stages)

    assert doc["nodes"] == [
        {"id": "A", "label": "Alpha", "stage": "select", "cues": ["one", "two"]},
        {"id": "B", "label": "Beta", "stage": "assurance", "cues": []},
    ]
    assert doc["excluded"]["collisions"] == [{"cue": "shared", "nodes": ["A", "B"]}]
