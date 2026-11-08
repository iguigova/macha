#!/usr/bin/env python3
"""gist_mail.py — mail a rendered gist to the user, from and to the address
the profile's line "My email is <address>." states.

  gist_mail.py GIST.json GIST.pdf [--stamp FILE]
  gist_mail.py --dry-run GIST.json GIST.pdf   # print the message; send nothing
  gist_mail.py --self-test

The subject is the gist's title; the body is its text with the markup
stripped (subtitle, metrics, each panel's bullets, takeaway, note); the
PDF is attached. It logs in to smtp.gmail.com:587 over STARTTLS with the
Gmail app password in the project secret `gmail-app-password`
(`~/.claude/scripts/secret_file.sh`), a file that must be mode 600. A
send that succeeds writes "<UTC time> sent: <subject>" to the --stamp
file; any failure exits 1 naming it, and writes no stamp.
"""
import html
import json
import os
import re
import smtplib
import stat
import subprocess
import sys
import tempfile
from datetime import datetime, timezone
from email.message import EmailMessage
from pathlib import Path

sys.path.insert(0, str(Path.home() / ".claude/scripts"))

ROOT = Path(__file__).resolve().parents[2]
PROFILE = ROOT / "jobs/profile/profile.txt"
SMTP_HOST, SMTP_PORT = "smtp.gmail.com", 587
SECRET = "gmail-app-password"


class Refused(Exception):
    pass


def profile_address(profile: Path) -> str:
    for line in profile.read_text().splitlines():
        m = re.fullmatch(r"My email is (\S+@\S+)\.", line.strip())
        if m:
            return m.group(1)
    raise Refused(f'{profile} has no line "My email is <address>."')


def app_password() -> str:
    run = subprocess.run([str(Path.home() / ".claude/scripts/secret_file.sh"), SECRET],
                         cwd=ROOT, capture_output=True, text=True)
    if run.returncode != 0:
        raise Refused(run.stderr.strip() or f"no secret {SECRET}")
    path = Path(run.stdout.strip())
    if stat.S_IMODE(path.stat().st_mode) & 0o077:
        raise Refused(f"{path} is readable by others; chmod 600 it")
    return path.read_text().strip()


def plain(markup: str) -> str:
    return html.unescape(re.sub(r"<[^>]+>", "", markup)).strip()


def body_text(gist: dict) -> str:
    parts = [plain(gist["subtitle"]), ""]
    parts += [f"{plain(v)} — {plain(label)}" for v, label in gist["metrics"]]
    for col in gist["columns"]:
        parts += ["", plain(col["title"])]
        parts += [f"- {plain(b)}" for b in col["bullets"]]
    parts += ["", plain(gist["takeaway"])]
    if gist.get("note"):
        parts += ["", plain(gist["note"])]
    return "\n".join(parts) + "\n"


def message(gist: dict, pdf: Path, address: str) -> EmailMessage:
    msg = EmailMessage()
    msg["Subject"] = plain(gist["title"])
    msg["From"] = msg["To"] = address
    msg.set_content(body_text(gist))
    msg.add_attachment(pdf.read_bytes(), maintype="application", subtype="pdf",
                       filename=pdf.name)
    return msg


def send(msg: EmailMessage, password: str, smtp=smtplib.SMTP) -> None:
    with smtp(SMTP_HOST, SMTP_PORT, timeout=60) as s:
        s.starttls()
        s.login(msg["From"], password)
        s.send_message(msg)


def main(argv: list) -> int:
    dry = argv[:1] == ["--dry-run"]
    args = argv[1:] if dry else argv
    stamp = None
    if len(args) == 4 and args[2] == "--stamp":
        stamp = Path(args[3])
        args = args[:2]
    if len(args) != 2:
        print(__doc__.split("\n\n")[1], file=sys.stderr)
        return 2
    try:
        gist = json.loads(Path(args[0]).read_text())
        pdf = Path(args[1])
        if not pdf.is_file():
            raise Refused(f"no PDF at {pdf}")
        msg = message(gist, pdf, profile_address(PROFILE))
        if dry:
            print(f"Subject: {msg['Subject']}\nTo: {msg['To']}\nAttachment: {pdf.name}\n\n"
                  f"{body_text(gist)}", end="")
            return 0
        send(msg, app_password())
    except (Refused, OSError, KeyError, ValueError, smtplib.SMTPException) as e:
        print(f"gist_mail.py: {type(e).__name__}: {e}", file=sys.stderr)
        return 1
    if stamp:
        now = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%MZ")
        stamp.write_text(f"{now} sent: {msg['Subject']}\n")
    print(f"gist_mail.py: sent \"{msg['Subject']}\" to {msg['To']}", file=sys.stderr)
    return 0


def self_test() -> int:
    from tap import Tap
    t = Tap()
    gist = {"title": "Verification sets <b>team</b> speed", "subtitle": "Feed, 7 Nov",
            "metrics": [["10x", "top vs median"]],
            "columns": [{"title": "What mattered", "bullets": ["<b>CI:</b> fewer retries &amp; less"]},
                        {"title": "B", "bullets": ["b"]}, {"title": "C", "bullets": ["c"]}],
            "takeaway": "Read one thing.", "note": "Figures are the posts'."}
    with tempfile.TemporaryDirectory() as d:
        d = Path(d)
        prof = d / "profile.txt"
        prof.write_text("My name is A.\nMy email is a@example.com.\n")
        t.check(profile_address(prof) == "a@example.com", "the address is the profile's email line")
        (d / "none.txt").write_text("My name is A.\n")
        try:
            profile_address(d / "none.txt")
            t.check(False, "a profile with no email line is refused")
        except Refused:
            t.check(True, "a profile with no email line is refused")

        body = body_text(gist)
        t.check("- CI: fewer retries & less" in body, "bullets lose their markup and unescape", body)
        t.check(body.index("What mattered") < body.index("Read one thing.") < body.index("Figures"),
                "the body runs panels, then the takeaway, then the note", body)

        pdf = d / "x_gist.pdf"
        pdf.write_bytes(b"%PDF-1.4 fake")
        msg = message(gist, pdf, "a@example.com")
        t.check(msg["Subject"] == "Verification sets team speed", "the subject is the title, unmarked", msg["Subject"])
        t.check(msg["From"] == msg["To"] == "a@example.com", "it is from and to the profile's address")
        att = [p for p in msg.iter_attachments()]
        t.check(len(att) == 1 and att[0].get_filename() == "x_gist.pdf"
                and att[0].get_content() == b"%PDF-1.4 fake", "the PDF is attached whole")

        calls = []

        class FakeSMTP:
            def __init__(self, host, port, timeout):
                calls.append(("connect", host, port))
            def __enter__(self): return self
            def __exit__(self, *a): return False
            def starttls(self): calls.append(("starttls",))
            def login(self, user, pw):
                calls.append(("login", user, pw))
                if pw != "good":
                    raise smtplib.SMTPAuthenticationError(535, b"bad")
            def send_message(self, m): calls.append(("send", m["Subject"]))

        send(msg, "good", FakeSMTP)
        t.check(calls == [("connect", "smtp.gmail.com", 587), ("starttls",),
                          ("login", "a@example.com", "good"), ("send", "Verification sets team speed")],
                "it logs in to Gmail over STARTTLS before it sends", repr(calls))
        calls.clear()
        try:
            send(msg, "bad", FakeSMTP)
            t.check(False, "a refused login raises and sends nothing")
        except smtplib.SMTPAuthenticationError:
            t.check(("send", "Verification sets team speed") not in calls,
                    "a refused login raises and sends nothing", repr(calls))
    return t.end()


if __name__ == "__main__":
    if sys.argv[1:] == ["--self-test"]:
        sys.exit(self_test())
    sys.exit(main(sys.argv[1:]))
