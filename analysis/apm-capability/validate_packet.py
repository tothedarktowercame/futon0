#!/usr/bin/env python3
"""Deterministic validator for mathematical-capability-packet.v1 packets.

Loads the normative schema (mathematical-capability-packet.v1.edn) and
validates packet EDN fixtures against it: required fields, types, enums,
patterns, min bounds, closed-world top-level keys, and the status-transition
invariants. Pure stdlib; output is a fixed sequence of lines so two runs on
the same inputs are byte-identical (run twice, diff must be empty).

Usage:
  python3 validate_packet.py                # validate all fixtures
  python3 validate_packet.py FILE...        # validate specific packet files
Exit 0 iff every fixture validates (positives pass, negatives refuse with
exactly their expected error kind recorded in fixtures/expectations.json).
"""
import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
SCHEMA_PATH = HERE / "mathematical-capability-packet.v1.edn"
FIXTURES = HERE / "fixtures"
EXPECTATIONS = FIXTURES / "expectations.json"


class EdnError(ValueError):
    pass


def parse_edn(text):
    """Parse the EDN subset used by the schema and fixtures: keywords,
    strings, integers, booleans, nil, maps, vectors, sets. Comments (;) and
    commas (whitespace) are handled. Returns Python values; keywords become
    strings of form ':name'."""
    pos = 0
    n = len(text)

    def skip_ws():
        nonlocal pos
        while pos < n:
            c = text[pos]
            if c in ", \t\r\n":
                pos += 1
            elif c == ";":
                while pos < n and text[pos] != "\n":
                    pos += 1
            else:
                return

    def parse_value():
        skip_ws()
        if pos >= n:
            raise EdnError("unexpected end of input")
        c = text[pos]
        if c == "{":
            return parse_map()
        if c == "[":
            return parse_vector()
        if text.startswith("#{", pos):
            return parse_set()
        if text[pos] == "#" and not text.startswith("#{", pos):
            raise EdnError("sets not supported in this position")
        if c == '"':
            return parse_string()
        if c == ":":
            return parse_keyword()
        tok = parse_token()
        if tok == "true":
            return True
        if tok == "false":
            return False
        if tok == "nil":
            return None
        try:
            return int(tok)
        except ValueError:
            raise EdnError(f"unsupported token: {tok!r}")

    def parse_map():
        nonlocal pos
        pos += 1
        out = {}
        while True:
            skip_ws()
            if pos < n and text[pos] == "}":
                pos += 1
                return out
            key = parse_value()
            skip_ws()
            if pos >= n or text[pos] not in "{}[\"#:" and not text[pos].isalnum():
                raise EdnError("expected map value")
            val = parse_value()
            out[key] = val

    def parse_vector():
        nonlocal pos
        pos += 1
        out = []
        while True:
            skip_ws()
            if pos < n and text[pos] == "]":
                pos += 1
                return out
            out.append(parse_value())

    def parse_set():
        nonlocal pos
        pos += 2  # "#{"
        out = set()
        while True:
            skip_ws()
            if pos < n and text[pos] == "}":
                pos += 1
                return out
            out.add(parse_value())

    def parse_string():
        nonlocal pos
        pos += 1
        buf = []
        while pos < n:
            c = text[pos]
            if c == "\\":
                pos += 1
                if pos >= n:
                    raise EdnError("unterminated escape")
                buf.append(text[pos])
                pos += 1
            elif c == '"':
                pos += 1
                return "".join(buf)
            else:
                buf.append(c)
                pos += 1
        raise EdnError("unterminated string")

    def parse_keyword():
        nonlocal pos
        start = pos
        pos += 1
        while pos < n and (text[pos].isalnum() or text[pos] in "/-_.*+!?<>="):
            pos += 1
        return text[start:pos]

    def parse_token():
        nonlocal pos
        start = pos
        while pos < n and text[pos] not in ", \t\r\n{}[]()\"":
            pos += 1
        return text[start:pos]

    # Top-level dispatch allowing sets (schema uses #{...}).
    skip_ws()
    if text.startswith("#{", pos):
        return parse_set()
    return parse_value()


TYPE_CHECKS = {
    "keyword": lambda v: isinstance(v, str) and v.startswith(":"),
    "string": lambda v: isinstance(v, str) and not v.startswith(":"),
    "integer": lambda v: isinstance(v, int) and not isinstance(v, bool),
    "map": lambda v: isinstance(v, dict),
    "vector": lambda v: isinstance(v, list),
}


class Refusal(Exception):
    def __init__(self, kind, path):
        self.kind = kind
        self.path = path
        super().__init__(f"{kind} at {path}")


def load_schema():
    return parse_edn(SCHEMA_PATH.read_text())


def statuses(schema):
    return {s.lstrip(":") if isinstance(s, str) else s for s in schema[":statuses"]}


def check_field(spec, value, path, schema, refusal):
    t = spec.get(":type")
    if not TYPE_CHECKS[t](value):
        refusal("type-mismatch", path)
        return
    if t == "keyword":
        raw = value[1:] if value.startswith(":") else value
        if spec.get(":enum") == ":statuses" and raw not in statuses(schema):
            refusal("enum-mismatch", path)
        if ":pattern" in spec and not re.fullmatch(spec[":pattern"], raw):
            refusal("pattern-mismatch", path)
    if t == "string" and ":pattern" in spec:
        if not re.fullmatch(spec[":pattern"], value):
            refusal("pattern-mismatch", path)
    if t == "integer" and ":min" in spec and value < spec[":min"]:
        refusal("min-violation", path)
    if t == "map" and ":fields" in spec:
        check_object(spec[":fields"], value, path, schema, refusal)
    if t == "vector":
        if ":min-length" in spec and len(value) < spec[":min-length"]:
            refusal("min-length-violation", path)
        item_spec = {":type": spec.get(":item-type")}
        if spec.get(":item-type") == "map" and ":item-fields" in spec:
            item_spec[":fields"] = spec[":item-fields"]
        for i, item in enumerate(value):
            check_field(item_spec, item, f"{path}[{i}]", schema, refusal)


def check_object(fields, obj, path, schema, refusal):
    if not isinstance(obj, dict):
        refusal("type-mismatch", path)
        return
    for name, spec in fields.items():
        if name not in obj:
            if spec.get(":required"):
                refusal("missing-required-field", f"{path}{name}")
            continue
        check_field(spec, obj[name], f"{path}{name}", schema, refusal)


def validate_packet(packet, schema):
    """Return list of (kind, path) refusals; empty list means valid."""
    refusals = []

    def refusal(kind, path):
        refusals.append((kind, path))

    if not isinstance(packet, dict):
        return [("type-mismatch", "$")]
    allowed = set(schema[":fields"])
    for key in packet:
        if key not in allowed:
            refusal("unknown-field", f"${key}")
    check_object(schema[":fields"], packet, "$", schema, refusal)
    if refusals:
        return refusals
    # Invariants (checked only when the structural fields survived).
    hist = packet.get(":packet/status-history")
    status = packet.get(":packet/status")
    if isinstance(hist, list) and hist and isinstance(status, str):
        head = hist[-1].get(":status")
        if isinstance(head, str) and head.lstrip(":") != status.lstrip(":"):
            refusal("history-head-mismatch", "$:packet/status-history[-1]")
        transitions = {k.lstrip(":"): {s.lstrip(":") for s in v}
                       for k, v in schema[":status-transitions"].items()}
        for i in range(len(hist) - 1):
            a = hist[i].get(":status")
            b = hist[i + 1].get(":status")
            if isinstance(a, str) and isinstance(b, str):
                if b.lstrip(":") not in transitions.get(a.lstrip(":"), set()):
                    refusal("illegal-status-transition",
                            f"$:packet/status-history[{i}->{i + 1}]")
    return refusals


def validate_file(path, schema):
    try:
        packet = parse_edn(path.read_text())
    except EdnError as exc:
        return [("edn-parse-error", f"$ ({exc})")]
    return validate_packet(packet, schema)


def main(argv):
    schema = load_schema()
    expectations = json.loads(EXPECTATIONS.read_text())
    if len(argv) > 1:
        files = [Path(a) for a in argv[1:]]
    else:
        files = sorted(p for p in FIXTURES.iterdir() if p.suffix == ".edn")
    failures = 0
    for f in files:
        refusals = validate_file(f, schema)
        rel = f.relative_to(HERE).as_posix()
        if rel in expectations:
            expected = expectations[rel]
            if not refusals:
                print(f"FAIL {rel}: expected refusal '{expected}', packet validated")
                failures += 1
            elif refusals[0][0] != expected:
                print(f"FAIL {rel}: expected refusal '{expected}', got "
                      f"'{refusals[0][0]}' at {refusals[0][1]}")
                failures += 1
            else:
                print(f"PASS {rel}: refused as expected '{expected}' at {refusals[0][1]}")
        else:
            if refusals:
                print(f"FAIL {rel}: {refusals[0][0]} at {refusals[0][1]}")
                failures += 1
            else:
                print(f"PASS {rel}")
    print(f"summary: {len(files)} fixtures, {failures} failures")
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
