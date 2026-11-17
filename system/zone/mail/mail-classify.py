#!/usr/bin/env python3
"""Tag freshly indexed mail (tag:new) by kind, for notmuch's post-new hook.

Kinds, first match wins:
  spam / deleted / draft / sent   by server folder (Spam, Trash, Drafts, Sent)
  github   from notifications@github.com; inbox only if it asks for you
  person   sender is someone you have written to (recipients of Sent), or
           matches ~/storage/mail/keep.txt                      -> inbox
  list     discussion list (has List-Post)                      -> not inbox
  bulk     newsletter/marketing (List-Unsubscribe or
           Precedence: bulk/list/junk)                          -> not inbox
  notify   machine mail from a no-reply style address           -> not inbox
  (other)  unknown sender                                       -> inbox

Only mail in the server INBOX ever gets +inbox; Archive mail is just indexed.
keep.txt and blocked.txt hold one address or @domain per line; @domain also
matches subdomains (@gov.uk covers @advice.hmrc.gov.uk). blocked.txt wins:
those senders get +bulk +blocked whatever their headers say.
Nothing is deleted or moved; everything here is a notmuch tag.
"""
import email.parser
import email.utils
import os
import re
import subprocess
import sys

MAILDIR = os.path.expanduser("~/storage/mail")
GITHUB_FOR_ME = {"mention", "review_requested", "assign", "team_mention", "author"}
# Matched anywhere in the local part: scholaralerts-noreply, no.reply, ...
NOREPLY = re.compile(
    r"no[-._]?reply|do[-._]?not[-._]?reply|notifications?|alerts?|automated"
    r"|mailer-daemon|newsletters?|marketing|digest|payments?-update"
    r"|shipment-tracking|^(news|updates?|daily|hello|hi|team|info|express)$",
    re.I,
)


def nm(*args):
    return subprocess.run(["notmuch", *args], check=True, capture_output=True,
                          text=True).stdout


def read_list(name):
    path = os.path.join(MAILDIR, name)
    if not os.path.exists(path):
        return set()
    with open(path) as fh:
        return {ln.strip().lower() for ln in fh
                if ln.strip() and not ln.startswith("#")}


def people():
    out = nm("address", "--output=recipients", "--deduplicate=address",
             "folder:hyperreal/Sent")
    addrs = {email.utils.parseaddr(ln)[1].lower() for ln in out.splitlines()}
    return addrs - {""}


def matches(addr, entries):
    if addr in entries:
        return True
    labels = addr.split("@")[-1].split(".")
    return any("@" + ".".join(labels[i:]) in entries for i in range(len(labels)))


def classify(path, hdr, ppl, keep, blocked):
    folder = os.path.relpath(path, MAILDIR).split(os.sep)[1]
    sender = email.utils.parseaddr(str(hdr.get("From", "")))[1].lower()
    if folder == "Spam":
        return ["+spam"]
    if folder == "Trash":
        return ["+deleted"]
    if folder == "Drafts":
        return ["+draft"]
    if folder == "Sent":
        return ["+sent"]
    in_inbox = folder == "INBOX"
    inbox = ["+inbox"] if in_inbox else []
    if sender == "notifications@github.com":
        reason = str(hdr.get("X-GitHub-Reason", "")).strip()
        return ["+github"] + (inbox if reason in GITHUB_FOR_ME else [])
    if matches(sender, blocked):
        return ["+bulk", "+blocked"]
    if sender in ppl or matches(sender, keep):
        return ["+person"] + inbox
    if hdr.get("List-Post"):
        return ["+list"]
    prec = str(hdr.get("Precedence", "")).lower()
    if (hdr.get("List-Unsubscribe") or prec in ("bulk", "list", "junk")
            ):
        return ["+bulk"]
    if NOREPLY.search(sender.split("@")[0]):
        return ["+notify"]
    return inbox


def main():
    ppl, keep, blocked = people(), read_list("keep.txt"), read_list("blocked.txt")
    files = [f for f in nm("search", "--output=files", "tag:new").splitlines() if f]
    parser = email.parser.BytesHeaderParser()
    batch, seen = [], set()
    for path in files:
        try:
            with open(path, "rb") as fh:
                hdr = parser.parse(fh)
        except OSError:
            continue
        mid = str(hdr.get("Message-ID", "")).strip().strip("<>").strip()
        if not mid or mid in seen:
            continue
        seen.add(mid)
        tags = classify(path, hdr, ppl, keep, blocked)
        if tags:
            batch.append(" ".join(tags) + ' -- id:"%s"' % mid.replace('"', '""'))
    if batch:
        subprocess.run(["notmuch", "tag", "--batch"], input="\n".join(batch) + "\n",
                       text=True, check=True)
    nm("tag", "-new", "--", "tag:new")
    # Mail moved to Trash on the server keeps its old tags (it is not new);
    # hide it here.
    nm("tag", "+deleted", "-inbox", "--", "folder:hyperreal/Trash and not tag:deleted")
    print(f"mail-classify: {len(seen)} new messages tagged", file=sys.stderr)


if __name__ == "__main__":
    main()
