#!/usr/bin/env python3
"""Send a desktop notification for Futon0 negative-space reminders."""

from __future__ import annotations

import argparse
import shutil
import subprocess
import sys


def notify(title: str, body: str, urgency: str | None) -> int:
    exe = shutil.which("notify-send")
    if not exe:
        print(f"[notify] {title}: {body}")
        return 0
    cmd = [exe]
    if urgency:
        cmd.extend(["--urgency", urgency])
    cmd.extend([title, body])
    return subprocess.call(cmd)


def main() -> None:
    parser = argparse.ArgumentParser(description="Emit a desktop reminder")
    parser.add_argument("--title", default="Futon reminder", help="Notification title")
    parser.add_argument("--body", required=True, help="Notification body text")
    parser.add_argument("--urgency", choices=["low", "normal", "critical"], default="normal")
    args = parser.parse_args()
    code = notify(args.title, args.body, args.urgency)
    sys.exit(code)


if __name__ == "__main__":
    main()
