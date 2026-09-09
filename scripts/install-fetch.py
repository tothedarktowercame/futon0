#!/usr/bin/env python3
"""Fetch a pinned public candidate into a new directory; never update existing trees."""
import argparse
import configparser
import hashlib
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import urllib.request


def validate(manifest):
    repos = manifest["repositories"]
    names = set()
    if manifest["schema_version"] != 1 or not repos:
        raise ValueError("unsupported or empty manifest")
    for repo in repos:
        name = repo["name"]
        if not re.fullmatch(r"futon[0-9]+[a-z]?", name) or name in names:
            raise ValueError("invalid or duplicate repository name: " + name)
        names.add(name)
        if repo["url"] != "https://github.com/tothedarktowercame/" + name + ".git":
            raise ValueError("repository URL must use the explicit public HTTPS authority")
        if not re.fullmatch(r"[0-9a-f]{40}", repo["commit"]):
            raise ValueError("full commit required: " + name)
        module_paths = set()
        for module in repo.get("submodules", []):
            path = module["path"]
            if (not re.fullmatch(r"[A-Za-z0-9_/-]+", path)
                    or any(p in ("", ".", "..") for p in path.split("/"))
                    or path in module_paths):
                raise ValueError("invalid submodule path: " + path)
            module_paths.add(path)
            if not re.fullmatch(r"https://github.com/[A-Za-z0-9_-]+/[A-Za-z0-9_.-]+", module["url"]):
                raise ValueError("submodule must use an explicit GitHub HTTPS URL")
            if not re.fullmatch(r"[0-9a-f]{40}", module["commit"]):
                raise ValueError("full submodule commit required")
    return repos


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--manifest", required=True, type=Path)
    parser.add_argument("--destination", required=True, type=Path)
    args = parser.parse_args()
    raw = args.manifest.read_bytes()
    repos = validate(json.loads(raw))
    destination = args.destination.absolute()
    if destination.exists() or destination.is_symlink():
        raise ValueError("destination already exists; refusing to merge or overwrite: " + str(destination))
    # Check every source anonymously before creating any checkout.
    for repo in [item for repo in repos for item in [repo, *repo.get("submodules", [])]]:
        github_path = repo["url"].removeprefix("https://github.com/").removesuffix(".git")
        request = urllib.request.Request(
            "https://api.github.com/repos/" + github_path,
            headers={"User-Agent": "futon-public-install"})
        with urllib.request.urlopen(request, timeout=30) as response:
            metadata = json.load(response)
        if metadata.get("private") is not False or metadata.get("clone_url") != "https://github.com/" + github_path + ".git":
            raise ValueError("public repository verification failed: " + github_path)
    destination.mkdir(parents=True)
    receipt_path = destination / "fetch-receipt.json"
    receipt = {"manifest_sha256": hashlib.sha256(raw).hexdigest(),
               "status": "in-progress", "repositories": [],
               "note": "Source fetch only; not runtime or release acceptance."}
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    env.update(GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL="/dev/null",
               GIT_TERMINAL_PROMPT="0", GIT_ASKPASS="/bin/false")

    def git(*command, cwd=None):
        return subprocess.check_output(["git", *command], cwd=cwd, env=env, text=True).strip()

    def record():
        receipt_path.write_text(json.dumps(receipt, indent=2) + "\n")

    record()
    try:
        for repo in repos:
            path = destination / repo["name"]
            print("Fetching", repo["name"], repo["commit"], flush=True)
            git("init", "--quiet", str(path))
            git("remote", "add", "origin", repo["url"], cwd=path)
            git("fetch", "--quiet", "--depth=1", "origin", repo["commit"], cwd=path)
            git("checkout", "--quiet", "--detach", repo["commit"], cwd=path)
            actual = git("rev-parse", "HEAD", cwd=path)
            if actual != repo["commit"] or git("status", "--porcelain", cwd=path):
                raise ValueError("checkout identity mismatch: " + repo["name"])
            gitlinks = {}
            for line in git("ls-tree", "-r", "HEAD", cwd=path).splitlines():
                metadata, name = line.split("\t", 1)
                if metadata.startswith("160000 "):
                    gitlinks[name] = metadata.split()[2]
            expected = {m["path"]: m["commit"] for m in repo.get("submodules", [])}
            if gitlinks != expected:
                raise ValueError("submodule pins differ from manifest: " + repo["name"])
            modules_file = path / ".gitmodules"
            configured = {}
            if modules_file.exists():
                config = configparser.ConfigParser(interpolation=None)
                config.read(modules_file)
                configured = {config[s]["path"]: config[s]["url"] for s in config.sections()}
            if configured != {m["path"]: m["url"] for m in repo.get("submodules", [])}:
                raise ValueError("submodule URLs differ from manifest: " + repo["name"])
            for module in repo.get("submodules", []):
                git("submodule", "update", "--init", "--depth=1", "--", module["path"], cwd=path)
                module_path = path / module["path"]
                if (git("rev-parse", "HEAD", cwd=module_path) != module["commit"]
                        or git("status", "--porcelain", cwd=module_path)
                        or (module_path / ".gitmodules").exists()):
                    raise ValueError("submodule not clean/pinned or contains undeclared nested modules")
            if git("status", "--porcelain", cwd=path):
                raise ValueError("checkout changed while fetching submodules")
            receipt["repositories"].append({"name": repo["name"], "commit": actual,
                                             "submodules": repo.get("submodules", [])})
            record()
        receipt["status"] = "source-fetch-passed"
        record()
    except BaseException:
        receipt["status"] = "failed"
        record()
        raise


if __name__ == "__main__":
    try:
        main()
    except (OSError, ValueError, KeyError, subprocess.CalledProcessError) as exc:
        print("ERROR:", exc, file=sys.stderr)
        sys.exit(1)
