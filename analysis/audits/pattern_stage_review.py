"""Blind pattern-stage review: validated local saves and categorical agreement."""
from __future__ import annotations

import json
import os
from pathlib import Path
import tempfile

KINDS = ("practice", "subject", "mixed")
STAGES = ("perceive", "believe", "evaluate", "select", "act", "assurance", "coordination", "none")


def validate_labels(labels, expected_ids):
    if not isinstance(labels, dict) or set(labels) != set(expected_ids):
        raise ValueError("Labels must contain exactly the blind sample IDs.")
    for values in labels.values():
        if not isinstance(values, dict) or set(values) != {"kind", "stage"}:
            raise ValueError("Each card needs kind and stage fields.")
        if values["kind"] not in (*KINDS, None) or values["stage"] not in (*STAGES, None):
            raise ValueError("Unknown kind or stage; use the rubric dropdowns.")
    return labels


def save_labels(path, labels, expected_ids):
    """Save only explicit human submissions; partial labels remain incomplete."""
    validate_labels(labels, expected_ids)
    path = Path(path)
    payload = {"schema": "pattern-stage-human-labels/v1", "labels": labels}
    temporary = None
    try:
        with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", dir=path.parent,
                                         prefix=path.name + ".", suffix=".tmp", delete=False) as stream:
            temporary = stream.name
            json.dump(payload, stream, indent=2, ensure_ascii=False)
            stream.write("\n")
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, path)
    finally:
        if temporary and os.path.exists(temporary):
            os.unlink(temporary)
    return payload


def load_labels(path, expected_ids):
    path = Path(path)
    if not path.exists():
        return {i: {"kind": None, "stage": None} for i in expected_ids}
    payload = json.loads(path.read_text(encoding="utf-8"))
    if payload.get("schema") != "pattern-stage-human-labels/v1":
        raise ValueError("Unexpected human label file schema.")
    return validate_labels(payload["labels"], expected_ids)


def confusion_and_kappa(reference, observed, categories):
    if len(reference) != len(observed) or not reference:
        raise ValueError("Agreement needs two equally sized nonempty label lists.")
    matrix = [[0 for _ in categories] for _ in categories]
    index = {label: n for n, label in enumerate(categories)}
    for a, b in zip(reference, observed):
        matrix[index[a]][index[b]] += 1
    n = len(reference)
    observed_agreement = sum(matrix[i][i] for i in range(len(categories))) / n
    expected_agreement = sum(sum(matrix[i]) * sum(row[i] for row in matrix)
                             for i in range(len(categories))) / (n * n)
    kappa = None if expected_agreement == 1 else ((observed_agreement - expected_agreement)
                                                 / (1 - expected_agreement))
    return {"n": n, "agreement": observed_agreement, "kappa": kappa,
            "rows": [{"reference": name, **dict(zip(categories, matrix[i]))}
                     for i, name in enumerate(categories)]}


def agreement_report(reference, human):
    validate_labels(human, reference)
    complete = sum(all(v[field] is not None for field in ("kind", "stage")) for v in human.values())
    if complete != len(reference):
        return {"status": "awaiting labels", "complete": complete, "total": len(reference)}
    ids = list(reference)
    return {"status": "complete", "complete": complete, "total": len(reference),
            **{field: confusion_and_kappa([reference[i][field] for i in ids],
                                          [human[i][field] for i in ids], categories)
               for field, categories in (("kind", KINDS), ("stage", STAGES))}}
