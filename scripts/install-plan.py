#!/usr/bin/env python3
"""Read-only installation planning. No launch, package install, or filesystem writes."""
import argparse
import json
import os
from pathlib import Path
import pwd
import shutil
import socket
import sys
from urllib.parse import urlsplit


CATALOG = Path(__file__).resolve().parents[1] / "config/install-profiles.json"
PORTS = {"agency": 7270, "substrate_health": 7272,
         "substrate": 7273, "drawbridge": 6968}


def memory_budget(profile):
    return sum(s["heap_mib"] + s["direct_mib"]
               for s in profile["services"].values()) + profile["overhead_reserve_mib"]


def plan(catalog, profile_name, account, instance, ports, substrate_url=None):
    profile = catalog["profiles"][profile_name]
    if not instance or any(c not in "abcdefghijklmnopqrstuvwxyz0123456789-" for c in instance):
        raise ValueError("instance must contain only lowercase letters, digits and hyphens")
    ports = dict(ports)
    if profile["requires_external_substrate"]:
        url = urlsplit(substrate_url or "")
        if (url.scheme not in ("http", "https") or not url.hostname
                or url.username or url.password or url.query or url.fragment):
            raise ValueError("this profile requires --substrate-url without credentials/query/fragment")
        ports = {k: v for k, v in ports.items() if not k.startswith("substrate")}
    elif substrate_url:
        raise ValueError("local-substrate profiles derive their substrate URL from the port")
    if any(not 1024 <= p <= 65535 for p in ports.values()):
        raise ValueError("ports must be unprivileged TCP ports (1024..65535)")
    if len(set(ports.values())) != len(ports):
        raise ValueError("each listener needs a distinct port")
    home = Path(account.pw_dir)
    base = home / "futon-install" / instance
    return {
        "schema_version": 1, "profile": profile_name,
        "qualification": catalog["status"], "launch_ready": False,
        "account": account.pw_name, "uid": account.pw_uid, "instance": instance,
        "source_root": str(base / "code"),
        "config_root": str(home / ".config/futon" / instance),
        "state_root": str(home / ".local/state/futon" / instance),
        "store_root": str(home / ".local/share/futon" / instance / "store"),
        "bind_host": "127.0.0.1", "ports": ports,
        "agency_url": "http://127.0.0.1:" + str(ports["agency"]),
        "substrate_url": substrate_url or "http://127.0.0.1:" + str(ports["substrate"]),
        "memory": profile, "planning_budget_mib": memory_budget(profile),
        "malloc_arena_max": 2,
        "remaining_gates": [
            "Public source/data closure and release commits must be qualified.",
            "Runtime launchers must consume the selected budgets and paths coherently.",
            "Instance provenance and process guards must support the approved staging topology.",
            "Cold boot, write/read/search, restart and workload/resource tests must pass."
        ]
    }


def doctor(result):
    checks = []

    def add(name, ok, detail):
        checks.append({"check": name, "ok": bool(ok), "detail": detail})

    add("target-user", os.geteuid() == result["uid"],
        "Run as the target account to assess its PATH, ownership and access.")
    mem = {}
    for line in Path("/proc/meminfo").read_text().splitlines():
        parts = line.split()
        if parts[0] in ("MemTotal:", "MemAvailable:", "SwapTotal:", "SwapFree:"):
            mem[parts[0][:-1]] = int(parts[1]) // 1024
    add("available-memory", mem["MemAvailable"] >= result["planning_budget_mib"],
        {"host_mib": mem, "required_planning_mib": result["planning_budget_mib"],
         "note": "Swap is not counted as RAM; budget fit is not workload qualification."})
    for name, port in result["ports"].items():
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
                sock.bind((result["bind_host"], port))
            add("port-" + name, True, {"port": port, "reserved": False})
        except OSError as exc:
            add("port-" + name, False, {"port": port, "error": str(exc)})
    for name in ("source_root", "config_root", "state_root", "store_root"):
        path = Path(result[name])
        parent = path.parent
        while not parent.exists():
            parent = parent.parent
        add(name + "-fresh", not path.exists() and not path.is_symlink(),
            {"path": str(path), "existing_parent": str(parent),
             "parent_writable_by_caller": os.access(parent, os.W_OK),
             "free_disk_mib": shutil.disk_usage(parent).free // (1024 * 1024),
             "note": "Disk requirement remains unqualified; existing destinations need inspection."})
    for tool in ("git", "curl", "java", "clojure", "bb", "clj-kondo",
                 "python3", "make", "systemctl"):
        found = shutil.which(tool)
        add("tool-" + tool, found is not None, found)
    return checks


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("plan", "doctor"))
    parser.add_argument("--profile", default="compact-8g")
    parser.add_argument("--user", default=pwd.getpwuid(os.geteuid()).pw_name)
    parser.add_argument("--instance", default="trial")
    parser.add_argument("--substrate-url")
    for name, port in PORTS.items():
        parser.add_argument("--" + name.replace("_", "-") + "-port", type=int, default=port)
    args = parser.parse_args()
    try:
        catalog = json.loads(CATALOG.read_text())
        result = plan(catalog, args.profile, pwd.getpwnam(args.user), args.instance,
                      {n: getattr(args, n + "_port") for n in PORTS}, args.substrate_url)
        if args.command == "doctor":
            result["checks"] = doctor(result)
        print(json.dumps(result, indent=2))
        # A plan can be generated successfully. Doctor cannot attest readiness yet.
        return 2 if args.command == "doctor" else 0
    except (KeyError, ValueError, OSError) as exc:
        parser.error(str(exc))


if __name__ == "__main__":
    sys.exit(main())
