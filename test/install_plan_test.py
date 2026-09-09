import importlib.util
import json
from pathlib import Path
import pwd
import os
import socket
import unittest

spec = importlib.util.spec_from_file_location(
    "install_plan", Path(__file__).resolve().parents[1] / "scripts/install-plan.py")
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class InstallPlanTest(unittest.TestCase):
    def setUp(self):
        self.catalog = json.loads(module.CATALOG.read_text())
        self.account = pwd.getpwuid(os.geteuid())

    def make_plan(self, profile="compact-8g", **kwargs):
        return module.plan(self.catalog, profile, self.account, "test-trial",
                           module.PORTS, **kwargs)

    def test_small_complete_budget_fits_eight_gib_but_not_four(self):
        result = self.make_plan()
        self.assertEqual(result["planning_budget_mib"], 6528)
        self.assertGreater(result["planning_budget_mib"], 4096)
        self.assertLess(result["planning_budget_mib"], 8192)
        self.assertFalse(result["launch_ready"])

    def test_external_substrate_is_required_and_not_silently_local(self):
        with self.assertRaises(ValueError):
            self.make_plan("agency-4g")
        result = self.make_plan("agency-4g", substrate_url="https://substrate.example")
        self.assertNotIn("substrate", result["ports"])
        self.assertNotIn("futon1b", result["memory"]["services"])
        with self.assertRaises(ValueError):
            self.make_plan("agency-4g", substrate_url="https://user:secret@example.com")

    def test_duplicate_ports_and_path_escape_are_rejected(self):
        for instance, ports in [("../joe", module.PORTS),
                                ("trial", dict(module.PORTS, agency=7273))]:
            with self.assertRaises(ValueError):
                module.plan(self.catalog, "compact-8g", self.account, instance, ports)

    def test_doctor_detects_actual_port_conflict_without_creating_paths(self):
        result = self.make_plan()
        with socket.socket() as listener:
            listener.bind(("127.0.0.1", 0))
            listener.listen()
            result["ports"] = {"agency": listener.getsockname()[1]}
            before = {k: Path(result[k]).exists() for k in
                      ("source_root", "config_root", "state_root", "store_root")}
            checks = module.doctor(result)
            check = next(c for c in checks if c["check"] == "port-agency")
            self.assertFalse(check["ok"])
            for key, existed in before.items():
                self.assertEqual(Path(result[key]).exists(), existed)


if __name__ == "__main__":
    unittest.main()
