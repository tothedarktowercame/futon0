import importlib.util
from pathlib import Path


SCRIPT = Path(__file__).resolve().parents[5] / "scripts/euclid_smt_check.py"
SPEC = importlib.util.spec_from_file_location("euclid_smt_check", SCRIPT)
smt = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(smt)


def test_i2_eight_moves_follows_but_seven_do_not():
    seven, raw7 = smt.replay_grounded(2, smt.I2_WITNESS[:7], 10)
    eight, raw8 = smt.replay_grounded(2, smt.I2_WITNESS, 10)
    assert seven.goal_reached() is False
    assert eight.goal_reached() is True
    # cvc5 does not establish satisfiability of the seven-move query under
    # the full quantified System E theory within the fixed timeout.  It must
    # not falsely report that the goal follows.
    assert smt.run_solver(smt.build_smt(2, seven, raw7), timeout=2)[0] in {
        "sat", "unknown", "timeout"
    }
    assert smt.run_solver(smt.build_smt(2, eight, raw8))[0] == "follows"
