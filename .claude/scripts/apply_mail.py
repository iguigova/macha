#!/usr/bin/env python3
"""apply_mail.py — send a job application by email, from the address the
profile's line "My email is <address>." states, to the address a posting
gives.

  apply_mail.py --to ADDR --subject TEXT --body FILE --attach PDF [--attach PDF …]
  apply_mail.py --dry-run --to … # print the message and its attachments; send nothing
  apply_mail.py --self-test

The body is FILE's text, sent as plain text. Each --attach is a PDF, sent
as application/pdf under its own file name. It sends through
digest_mail.py's Gmail login, smtp.gmail.com:587 over STARTTLS with the
project secret `gmail-app-password`, so Gmail keeps the copy in Sent.
A refusal or a failed send exits 1 naming it, and sends nothing.
"""
import argparse
import re
import smtplib
import sys
import tempfile
from email.message import EmailMessage
from pathlib import Path

sys.dont_write_bytecode = True   # the gate reads every entry in scripts/ as a script
sys.path.insert(0, str(Path(__file__).resolve().parent))
sys.path.insert(0, str(Path.home() / ".claude/scripts"))
from digest_mail import PROFILE, Refused, app_password, profile_address, send  # noqa: E402


def message(sender: str, to: str, subject: str, body: str, attachments: list) -> EmailMessage:
    if not re.fullmatch(r"[^@\s]+@[^@\s]+\.[^@\s]+", to):
        raise Refused(f"{to!r} is not an email address")
    if not subject.strip():
        raise Refused("the subject is empty")
    if not body.strip():
        raise Refused("the body is empty")
    if not attachments:
        raise Refused("no attachment: an application carries the resume")
    msg = EmailMessage()
    msg["Subject"] = subject
    msg["From"] = sender
    msg["To"] = to
    msg.set_content(body)
    for path in attachments:
        path = Path(path)
        data = path.read_bytes()
        if path.suffix.lower() != ".pdf" or not data.startswith(b"%PDF"):
            raise Refused(f"{path} is not a PDF")
        msg.add_attachment(data, maintype="application", subtype="pdf", filename=path.name)
    return msg


def describe(msg: EmailMessage) -> str:
    names = [f"{p.get_filename()} ({len(p.get_content())} bytes)" for p in msg.iter_attachments()]
    return (f"From: {msg['From']}\nTo: {msg['To']}\nSubject: {msg['Subject']}\n"
            f"Attachments: {', '.join(names)}\n\n{msg.get_body(('plain',)).get_content()}")


def main(argv: list) -> int:
    ap = argparse.ArgumentParser(prog="apply_mail.py", add_help=False)
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--to", required=True)
    ap.add_argument("--subject", required=True)
    ap.add_argument("--body", required=True)
    ap.add_argument("--attach", action="append", default=[])
    try:
        a = ap.parse_args(argv)
    except SystemExit:
        print(__doc__.split("\n\n")[1], file=sys.stderr)
        return 2
    try:
        msg = message(profile_address(PROFILE), a.to, a.subject, Path(a.body).read_text(), a.attach)
        if a.dry_run:
            print(describe(msg), end="")
            return 0
        send(msg, app_password())
    except (Refused, OSError, smtplib.SMTPException) as e:
        print(f"apply_mail.py: {type(e).__name__}: {e}", file=sys.stderr)
        return 1
    print(f"apply_mail.py: sent \"{msg['Subject']}\" to {msg['To']}", file=sys.stderr)
    return 0


def self_test() -> int:
    from tap import Tap
    t = Tap()
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        pdf = tmp / "A_Resume.pdf"
        pdf.write_bytes(b"%PDF-1.4 fake")
        txt = tmp / "notes.pdf"
        txt.write_bytes(b"not a pdf")

        msg = message("a@example.com", "hiring@example.org", "Role — A", "Dear team,\n", [pdf])
        t.check(msg["From"] == "a@example.com" and msg["To"] == "hiring@example.org",
                "it is from the given sender to the posting's address")
        t.check(msg["Subject"] == "Role — A", "the subject is the one given")
        t.check(msg.get_body(("plain",)).get_content() == "Dear team,\n", "the body is the file's text")
        att = list(msg.iter_attachments())
        t.check([(p.get_content_type(), p.get_filename()) for p in att] == [("application/pdf", "A_Resume.pdf")]
                and att[0].get_content() == b"%PDF-1.4 fake",
                "each attachment is the PDF itself under its own name", repr(att))
        t.check("Attachments: A_Resume.pdf (13 bytes)" in describe(msg) and describe(msg).endswith("Dear team,\n"),
                "the dry run shows the headers, the attachments and the body", describe(msg))

        for name, args in [("a bad address is refused", ("nobody", "S", "B", [pdf])),
                           ("an empty subject is refused", ("h@x.org", " ", "B", [pdf])),
                           ("an empty body is refused", ("h@x.org", "S", "\n", [pdf])),
                           ("a message with no attachment is refused", ("h@x.org", "S", "B", [])),
                           ("an attachment that is not a PDF is refused", ("h@x.org", "S", "B", [txt]))]:
            try:
                message("a@example.com", *args)
                t.check(False, name)
            except Refused:
                t.check(True, name)

    calls = []

    class FakeSMTP:
        def __init__(self, host, port, timeout):
            calls.append(("connect", host, port))
        def __enter__(self): return self
        def __exit__(self, *a): return False
        def starttls(self): calls.append(("starttls",))
        def login(self, user, pw): calls.append(("login", user))
        def send_message(self, m): calls.append(("send", m["To"]))

    send(msg, "pw", FakeSMTP)
    t.check(calls == [("connect", "smtp.gmail.com", 587), ("starttls",), ("login", "a@example.com"),
                      ("send", "hiring@example.org")],
            "it logs in to Gmail as the sender and sends to the posting's address", repr(calls))
    return t.end()


if __name__ == "__main__":
    if sys.argv[1:] == ["--self-test"]:
        sys.exit(self_test())
    if sys.argv[1:2] in (["-h"], ["--help"]):
        print(__doc__)
        sys.exit(0)
    sys.exit(main(sys.argv[1:]))
