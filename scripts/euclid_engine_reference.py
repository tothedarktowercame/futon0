"""Frozen pre-optimization arithmetic query path for equivalence tests.

The engine itself is shared deliberately: the speed change is confined to
``LinearArithmetic.entails``.  This module freezes that former method and a
State whose arithmetic store uses it, so randomized traces compare the old
decision path with the optimized one without maintaining a second 5,000-line
rule table.
"""

from fractions import Fraction

import euclid_engine as engine

_ENT_GLOBAL = {}


class LinearArithmetic(engine.LinearArithmetic):
    """LinearArithmetic with the pre-optimization ``entails`` method."""

    def entails(self, coeffs, const, op):
        if op in (">", ">="):
            coeffs, const, op = ({k: -v for k, v in coeffs.items()}, -const,
                                 "<" if op == ">" else "<=")
        q = (tuple(sorted((a, c) for a, c in coeffs.items() if c != 0)),
             const, op)
        if getattr(self, "_ent_rows", None) is not self.rows:
            self._ent = {}
            self._ent_rows = self.rows
        key = (len(self.rows), q)
        hit = self._ent.get(key)
        if hit is not None:
            return hit
        gkey = (tuple(self.rows), q)
        hit = _ENT_GLOBAL.get(gkey)
        if hit is not None:
            self._ent[key] = hit
            return hit
        items, c = engine.norm(coeffs, -Fraction(const))
        if (items, c, op) in self.rows or \
                (op == "<=" and (items, c, "=") in self.rows):
            self._ent[key] = True
            _ENT_GLOBAL[gkey] = True
            return True
        result = self._entails_run(coeffs, const, op)
        self._ent[key] = result
        if len(_ENT_GLOBAL) > 500000:
            _ENT_GLOBAL.clear()
        _ENT_GLOBAL[gkey] = result
        return result


class State(engine.State):
    def __init__(self):
        super().__init__()
        self.la = LinearArithmetic()

    def copy(self):
        state = State()
        state.facts = dict(self.facts)
        state.fresh = self.fresh
        state.declared = set(self.declared)
        state.reserved = set(self.reserved)
        state.la.rows = list(self.la.rows)
        state.rule_cache = dict(getattr(self, "rule_cache", {}))
        state.exclude_rules = self.exclude_rules
        state.proof_branch = self.proof_branch
        return state


def from_state(source):
    """Make a reference state with the same observable engine contents."""
    state = State()
    state.facts = dict(source.facts)
    state.fresh = source.fresh
    state.declared = set(source.declared)
    state.reserved = set(source.reserved)
    state.la.rows = list(source.la.rows)
    state.rule_cache = dict(getattr(source, "rule_cache", {}))
    state.exclude_rules = source.exclude_rules
    state.proof_branch = source.proof_branch
    return state
