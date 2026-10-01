import os
import subprocess
import tempfile
import unittest
from pathlib import Path


REPO = Path(__file__).resolve().parents[1]
SCRIPT = REPO / "scripts" / "provision-futon-demo.sh"
FUTON3C = REPO.parent / "futon3c"


class ProvisionFutonDemoTest(unittest.TestCase):
    def test_smoke_resolves_real_dev_serve_classpath_without_starting_its_main(self):
        """The exact 2026-10-01 failure was -M:dev-serve executing futon3c.dev."""
        result = subprocess.run(
            ["clojure", "-Spath", "-M:dev-serve"],
            cwd=FUTON3C,
            check=True,
            capture_output=True,
            text=True,
            timeout=30,
        )

        self.assertIn("clojure-", result.stdout)
        self.assertNotIn("[dev]", result.stdout)
        self.assertNotIn("[dev]", result.stderr)

        script = SCRIPT.read_text()
        self.assertIn('clojure -Spath -M:dev-serve', script)
        self.assertIn('-cp "$smoke_classpath" clojure.main -e', script)
        self.assertNotIn('clojure -M:dev-serve -P', script)
        self.assertNotIn('clojure -M:dev-serve -m clojure.main', script)

    def test_demo_ports_are_validated_for_range_distinctness_and_tcp_udp_use(self):
        script = SCRIPT.read_text()

        self.assertIn("p < 1 || p > 65535", script)
        self.assertIn("both select port", script)
        self.assertIn('ss -H -ltn "sport = :$p"', script)
        self.assertIn('ss -H -lun "sport = :$p"', script)

        with tempfile.TemporaryDirectory() as demo_root:
            env = os.environ | {
                "DEMO_ROOT": demo_root,
                "SRC_BASE": str(REPO.parent),
                "AGENCY_PORT": "17070",
                "SUBSTRATE_PORT": "17070",
                "DRAWBRIDGE_PORT": "16768",
            }
            result = subprocess.run(
                ["bash", str(SCRIPT)],
                env=env,
                capture_output=True,
                text=True,
                timeout=10,
            )

        self.assertEqual(3, result.returncode)
        self.assertIn("both select port 17070", result.stderr)

    def test_generated_environment_confines_watcher_roots_to_demo_checkout(self):
        script = SCRIPT.read_text()

        self.assertIn(
            'FUTON3C_INSTALLATION_WATCH_ROOT="$DEMO_ROOT/code"', script
        )


if __name__ == "__main__":
    unittest.main()
