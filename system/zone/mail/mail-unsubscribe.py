#!/usr/bin/env python3
"""One-click unsubscribe (RFC 8058) for the given sender addresses.

For each address: take its newest message, and if it carries both
List-Unsubscribe (with an https URL) and List-Unsubscribe-Post, POST
"List-Unsubscribe=One-Click" to that URL. Nothing is sent by mail.
Every sender is also appended to ~/storage/mail/blocked.txt, so later mail
from it is tagged +bulk +blocked even if the unsubscribe is ignored.
Results go to ~/storage/mail/unsubscribed.tsv.

usage: mail-unsubscribe.py ADDRESS...   (or --dry-run ADDRESS...)
"""
import datetime
import email.parser
import os
import re
import subprocess
import sys
import urllib.request

MAILDIR = os.path.expanduser("~/storage/mail")


def newest_file(addr):
    out = subprocess.run(
        ["notmuch", "search", "--output=files", "--sort=newest-first",
         "--limit=1", f"from:{addr}"], capture_output=True, text=True, check=True)
    return out.stdout.strip().splitlines()[0] if out.stdout.strip() else None


def unsubscribe(addr, dry):
    path = newest_file(addr)
    if not path:
        return "no-mail", ""
    with open(path, "rb") as fh:
        hdr = email.parser.BytesHeaderParser().parse(fh)
    lu = re.sub(r"\s+", "", str(hdr.get("List-Unsubscribe", "")))
    urls = re.findall(r"<(https://[^>]+)>", lu)
    if not urls:
        return "no-https-url", lu[:120]
    if "one-click" not in str(hdr.get("List-Unsubscribe-Post", "")).lower():
        return "not-one-click", urls[0]
    if dry:
        return "dry-run", urls[0]
    req = urllib.request.Request(
        urls[0], data=b"List-Unsubscribe=One-Click", method="POST",
        headers={"Content-Type": "application/x-www-form-urlencoded",
                 "User-Agent": "mail-unsubscribe/1"})
    try:
        with urllib.request.urlopen(req, timeout=20) as resp:
            return f"http-{resp.status}", urls[0]
    except urllib.error.HTTPError as e:
        return f"http-{e.code}", urls[0]
    except Exception as e:  # report every failure, never drop it
        return f"error-{type(e).__name__}", urls[0]


def main(argv):
    dry = "--dry-run" in argv
    addrs = [a.lower() for a in argv if a != "--dry-run"]
    blocked_path = os.path.join(MAILDIR, "blocked.txt")
    with open(blocked_path) as fh:
        blocked = {ln.strip().lower() for ln in fh}
    now = datetime.datetime.now().isoformat(timespec="seconds")
    for addr in addrs:
        status, url = unsubscribe(addr, dry)
        print(f"{status:16} {addr}")
        if dry:
            continue
        with open(os.path.join(MAILDIR, "unsubscribed.tsv"), "a") as log:
            log.write(f"{now}\t{addr}\t{status}\t{url}\n")
        if addr not in blocked:
            with open(blocked_path, "a") as fh:
                fh.write(addr + "\n")
            blocked.add(addr)


if __name__ == "__main__":
    main(sys.argv[1:])
