import json
from pathlib import Path
import subprocess
import tempfile
import unittest


SCRIPT = Path(__file__).parents[1] / "scripts" / "futon-recovery-backup.py"


def sh(*args, cwd=None):
    return subprocess.run(args, cwd=cwd, text=True, check=True,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE).stdout.strip()


class RecoveryBackupTest(unittest.TestCase):
    def test_three_worktrees_get_distinct_wip_refs_on_two_hubs(self):
        with tempfile.TemporaryDirectory(prefix="recovery-backup-") as td:
            tmp_path = Path(td)
            code = tmp_path / "code"
            code.mkdir()
            repo = code / "project"
            sh("git", "init", "-b", "main", str(repo))
            sh("git", "config", "user.name", "Fixture", cwd=repo)
            sh("git", "config", "user.email", "fixture@example.invalid", cwd=repo)
            (repo / "base.txt").write_text("base\n")
            sh("git", "add", "base.txt", cwd=repo)
            sh("git", "commit", "-m", "base", cwd=repo)
            wt1, wt2 = tmp_path / "worktrees" / "one", tmp_path / "worktrees" / "two"
            wt1.parent.mkdir()
            sh("git", "worktree", "add", "-b", "one", str(wt1), cwd=repo)
            sh("git", "worktree", "add", "-b", "two", str(wt2), cwd=repo)
            (repo / "main-wip.txt").write_text("main\n")
            (wt1 / "one-wip.txt").write_text("one\n")
            (wt2 / "two-wip.txt").write_text("two\n")
            hub1, hub2 = tmp_path / "hub1", tmp_path / "hub2"
            legacy = sh("git", "rev-parse", "HEAD", cwd=repo)
            sh("git", "update-ref", "refs/backup/wip", legacy, cwd=repo)
            for hub in (hub1, hub2):
                bare = hub / "project.git"
                sh("git", "init", "--bare", str(bare))
                sh("git", "push", str(bare),
                   "+refs/backup/wip:refs/backup/wip", cwd=repo)
            receipt = tmp_path / "receipt.json"
            sh("python3", str(SCRIPT), "--code", str(code),
               "--hub", str(hub1), "--hub", str(hub2), "--receipt", str(receipt))
            report = json.loads(receipt.read_text())
            rows = report["repos"][0]["worktrees"]
            refs = {row["ref"] for row in rows}
            self.assertEqual(3, len(rows))
            self.assertEqual(3, len(refs))
            self.assertTrue(all(row["status"] == "written" for row in rows))
            for hub in (hub1, hub2):
                bare = hub / "project.git"
                backed = set(sh("git", "for-each-ref", "--format=%(refname)",
                                "refs/backup/wip", cwd=bare).splitlines())
                self.assertEqual(refs, backed)
                self.assertEqual(legacy, sh("git", "rev-parse",
                                            "refs/backup/wip-legacy", cwd=bare))
                missing_legacy_name = subprocess.run(
                    ["git", "rev-parse", "-q", "--verify", "refs/backup/wip"],
                    cwd=bare).returncode
                self.assertNotEqual(0, missing_legacy_name)
            self.assertEqual(20 * 1024 * 1024,
                             report["repos"][0]["untracked-cutoff-bytes"])
            self.assertTrue(report["repos"][0]["legacy-wip-migrated"])

    def test_successful_unchanged_repo_skips_but_changed_wip_and_full_do_not(self):
        with tempfile.TemporaryDirectory(prefix="recovery-skip-") as td:
            root = Path(td)
            code, repo = root / "code", root / "code" / "project"
            code.mkdir()
            sh("git", "init", "-b", "main", str(repo))
            sh("git", "config", "user.name", "Fixture", cwd=repo)
            sh("git", "config", "user.email", "fixture@example.invalid", cwd=repo)
            (repo / "tracked").write_text("base\n")
            sh("git", "add", "tracked", cwd=repo)
            sh("git", "commit", "-m", "base", cwd=repo)
            (repo / "wip").write_text("one\n")
            hubs = [root / "hub1", root / "hub2"]
            state, receipt = root / "state.json", root / "receipt.json"

            def invoke(*extra):
                sh("python3", str(SCRIPT), "--code", str(code),
                   "--hub", str(hubs[0]), "--hub", str(hubs[1]),
                   "--state", str(state), "--receipt", str(receipt), *extra)
                return json.loads(receipt.read_text())

            first = invoke()
            self.assertEqual("full", first["repos"][0]["recovery-mode"])
            second = invoke()
            self.assertEqual("incremental", second["reconciliation"])
            self.assertEqual("unchanged-skip", second["repos"][0]["recovery-mode"])

            (repo / "wip").write_text("two\n")
            changed = invoke()
            self.assertEqual("full", changed["repos"][0]["recovery-mode"])
            forced = invoke("--full")
            self.assertEqual("full", forced["reconciliation"])
            self.assertEqual("full", forced["repos"][0]["recovery-mode"])

    def test_failed_hub_receipt_is_never_reused_for_skip(self):
        with tempfile.TemporaryDirectory(prefix="recovery-failed-") as td:
            root = Path(td)
            code, repo = root / "code", root / "code" / "project"
            code.mkdir()
            sh("git", "init", "-b", "main", str(repo))
            sh("git", "config", "user.name", "Fixture", cwd=repo)
            sh("git", "config", "user.email", "fixture@example.invalid", cwd=repo)
            (repo / "tracked").write_text("base\n")
            sh("git", "add", "tracked", cwd=repo)
            sh("git", "commit", "-m", "base", cwd=repo)
            bad_hub = root / "not-a-directory"
            bad_hub.write_text("occupied\n")
            state, receipt = root / "state.json", root / "receipt.json"
            args = ["python3", str(SCRIPT), "--code", str(code),
                    "--hub", str(root / "hub1"), "--hub", str(bad_hub),
                    "--state", str(state), "--receipt", str(receipt)]
            for expected_reconciliation in ("full", "incremental"):
                result = subprocess.run(args, text=True, stdout=subprocess.PIPE,
                                        stderr=subprocess.PIPE)
                self.assertNotEqual(0, result.returncode)
                report = json.loads(receipt.read_text())
                self.assertEqual(expected_reconciliation, report["reconciliation"])
                self.assertEqual("full", report["repos"][0]["recovery-mode"])
                self.assertFalse(report["repos"][0]["ok"])


if __name__ == "__main__":
    unittest.main()
