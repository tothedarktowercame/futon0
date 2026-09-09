import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest

SCRIPT = Path(__file__).resolve().parents[1] / "scripts/install-fetch.py"
spec = importlib.util.spec_from_file_location("install_fetch", SCRIPT)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class InstallFetchTest(unittest.TestCase):
    def fixture(self):
        return {"schema_version": 1, "repositories": [{
            "name": "futon0", "url": "https://github.com/tothedarktowercame/futon0.git",
            "commit": "a" * 40}]}

    def test_existing_destination_is_preserved_before_network(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            manifest = root / "manifest.json"
            manifest.write_text(json.dumps(self.fixture()))
            dest = root / "code"
            dest.mkdir()
            sentinel = dest / "keep"
            sentinel.write_text("existing work")
            result = subprocess.run([sys.executable, str(SCRIPT), "--manifest", str(manifest),
                                     "--destination", str(dest)], capture_output=True, text=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertIn("destination already exists", result.stderr)
            self.assertEqual(sentinel.read_text(), "existing work")
            self.assertEqual(list(dest.iterdir()), [sentinel])

    def test_rejects_escape_and_credential_urls(self):
        for patch in [{"name": "../futon0"}, {"url": "git@github.com:private/repo.git"},
                      {"commit": "main"},
                      {"submodules": [{"path": "../escape", "url": "https://github.com/a/b",
                                        "commit": "a" * 40}]}]:
            manifest = self.fixture()
            manifest["repositories"][0].update(patch)
            with self.assertRaises(ValueError):
                module.validate(manifest)


if __name__ == "__main__":
    unittest.main()
