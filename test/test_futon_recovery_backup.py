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
            self.assertEqual(20 * 1024 * 1024,
                             report["repos"][0]["untracked-cutoff-bytes"])


if __name__ == "__main__":
    unittest.main()
