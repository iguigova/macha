#!/usr/bin/env python3
"""digest_mail.py — mail a ranked digest to the user, from and to the
address the profile's line "My email is <address>." states.

  digest_mail.py DIGEST.json [--stamp FILE]
  digest_mail.py --dry-run DIGEST.json   # print the message; send nothing
  digest_mail.py --self-test

DIGEST.json is:

  {"title": "...",                  # the day's main claim: the subject
   "subtitle": "...",               # what was read, when, and its counts
   "items": [{"headline": "...", "source": "...", "what": "...",
              "why": "...", "link": "...",     # most consequential first;
              "section": "..."}],              # link may be ""; section is
                                               # optional, a heading over the
                                               # items that share it
   "why_label": "...",                         # optional; "Why it matters
                                               # to you" when absent
   "skip": ["<label>: <reason>", ...],         # may be []
   "note": "..."}                              # optional

The body is the list as plain text, with an HTML copy in which each
headline and each skip label is bold. It logs in to smtp.gmail.com:587
over STARTTLS with the Gmail app password in the project secret
`gmail-app-password` (`~/.claude/scripts/secret_file.sh`), a file that
must be mode 600. A send that succeeds writes "<local time and its UTC
offset> sent: <subject>" to the --stamp file; any failure exits 1 naming
it, and writes no stamp.
"""
import html
import json
import re
import smtplib
import stat
import subprocess
import sys
import tempfile
from datetime import datetime, timedelta, timezone
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


def split_label(line: str):
    label, sep, rest = line.partition(": ")
    return (label, rest) if sep else ("", line)


def why_label(d: dict) -> str:
    return d.get("why_label") or "Why it matters to you"


def body_text(d: dict) -> str:
    out = [d["subtitle"], ""]
    section = None
    for n, it in enumerate(d["items"], 1):
        if it.get("section") and it["section"] != section:
            section = it["section"]
            out += [section.upper(), ""]
        out.append(f"{n}. {it['headline']}")
        out += [f"   {it['source']}", f"   {it['what']}", f"   {why_label(d)}: {it['why']}"]
        if it.get("link"):
            out.append(f"   {it['link']}")
        out.append("")
    if d["skip"]:
        out.append("Safe to skip")
        out += [f"- {s}" for s in d["skip"]]
        out.append("")
    if d.get("note"):
        out.append(d["note"])
    return "\n".join(out).rstrip() + "\n"


def body_html(d: dict) -> str:
    e = html.escape
    parts = [f"<p>{e(d['subtitle'])}</p>", "<ol>"]
    section = None
    for n, it in enumerate(d["items"], 1):
        if it.get("section") and it["section"] != section:
            section = it["section"]
            parts += ["</ol>", f"<h3>{e(section)}</h3>", f'<ol start="{n}">']
        link = (f'<br><a href="{e(it["link"], quote=True)}">{e(it["link"])}</a>'
                if it.get("link") else "")
        parts.append(f"<li><p><b>{e(it['headline'])}</b><br>{e(it['source'])}<br>"
                     f"{e(it['what'])}<br><i>{e(why_label(d))}:</i> {e(it['why'])}{link}</p></li>")
    parts.append("</ol>")
    if d["skip"]:
        parts.append("<p><b>Safe to skip</b></p><ul>")
        for s in d["skip"]:
            label, rest = split_label(s)
            parts.append(f"<li><b>{e(label)}:</b> {e(rest)}</li>" if label else f"<li>{e(rest)}</li>")
        parts.append("</ul>")
    if d.get("note"):
        parts.append(f"<p><small>{e(d['note'])}</small></p>")
    return "<html><body>" + "\n".join(parts) + "</body></html>\n"


def message(d: dict, address: str) -> EmailMessage:
    if not d.get("items"):
        raise Refused("the digest has no items")
    msg = EmailMessage()
    msg["Subject"] = d["title"]
    msg["From"] = msg["To"] = address
    msg.set_content(body_text(d))
    msg.add_alternative(body_html(d), subtype="html")
    return msg


def send(msg: EmailMessage, password: str, smtp=smtplib.SMTP) -> None:
    with smtp(SMTP_HOST, SMTP_PORT, timeout=60) as s:
        s.starttls()
        s.login(msg["From"], password)
        s.send_message(msg)


def stamp_line(subject: str, now: datetime) -> str:
    return f"{now:%Y-%m-%d %H:%M %z} sent: {subject}\n"


def main(argv: list) -> int:
    dry = argv[:1] == ["--dry-run"]
    args = argv[1:] if dry else argv
    stamp = None
    if len(args) == 3 and args[1] == "--stamp":
        stamp = Path(args[2])
        args = args[:1]
    if len(args) != 1:
        print(__doc__.split("\n\n")[1], file=sys.stderr)
        return 2
    try:
        d = json.loads(Path(args[0]).read_text())
        msg = message(d, profile_address(PROFILE))
        if dry:
            print(f"Subject: {msg['Subject']}\nTo: {msg['To']}\n\n{body_text(d)}", end="")
            return 0
        send(msg, app_password())
    except (Refused, OSError, KeyError, TypeError, ValueError, smtplib.SMTPException) as e:
        print(f"digest_mail.py: {type(e).__name__}: {e}", file=sys.stderr)
        return 1
    if stamp:
        stamp.write_text(stamp_line(msg["Subject"], datetime.now().astimezone()))
    print(f"digest_mail.py: sent \"{msg['Subject']}\" to {msg['To']}", file=sys.stderr)
    return 0


def self_test() -> int:
    from tap import Tap
    t = Tap()
    d = {"title": "Top teams pull away on CI", "subtitle": "Feed, 7 Nov: 41 items",
         "items": [{"headline": "CI <gap> grew", "source": "Fabian Wesner (1d)",
                    "what": "Top 5% merge 10x more.", "why": "Interview metric.",
                    "link": "https://www.linkedin.com/feed/update/urn:li:activity:1/"},
                   {"headline": "Review vs agents", "source": "Ryan Daum (1w)",
                    "what": "Agents outpace review.", "why": "Your method.", "link": ""}],
         "skip": ["No job leads: the block was empty", "plain line"],
         "note": "Counts are from one scroll."}
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        prof = tmp / "profile.txt"
        prof.write_text("My name is A.\nMy email is a@example.com.\n")
        t.check(profile_address(prof) == "a@example.com", "the address is the profile's email line")
        (tmp / "none.txt").write_text("My name is A.\n")
        try:
            profile_address(tmp / "none.txt")
            t.check(False, "a profile with no email line is refused")
        except Refused:
            t.check(True, "a profile with no email line is refused")

    text = body_text(d)
    t.check(text.index("1. CI <gap> grew") < text.index("2. Review vs agents") < text.index("Safe to skip")
            < text.index("Counts are from one scroll."),
            "the text runs the items in rank, then what to skip, then the note", text)
    t.check("   https://www.linkedin.com/feed/update/urn:li:activity:1/" in text
            and text.count("https://") == 1, "an item's link is shown only when it has one", text)
    page = body_html(d)
    t.check("<b>CI &lt;gap&gt; grew</b>" in page and "<b>No job leads:</b> the block was empty" in page
            and "<li>plain line</li>" in page, "the HTML bolds headlines and skip labels, escaped", page)

    news = {**d, "why_label": "Who it affects",
            "items": [{**d["items"][0], "section": "World"}, {**d["items"][1], "section": "Science"}]}
    text = body_text(news)
    t.check(text.index("WORLD") < text.index("1. CI") < text.index("SCIENCE") < text.index("2. Review")
            and "Who it affects: Interview metric." in text and "Why it matters" not in text,
            "items carry their section headings and the digest's own why label", text)
    t.check('<h3>Science</h3>\n<ol start="2">' in body_html(news),
            "the HTML numbering runs on across sections", body_html(news))

    msg = message(d, "a@example.com")
    t.check(msg["Subject"] == "Top teams pull away on CI", "the subject is the title")
    t.check(msg["From"] == msg["To"] == "a@example.com", "it is from and to the profile's address")
    kinds = [p.get_content_type() for p in msg.iter_parts()]
    t.check(kinds == ["text/plain", "text/html"] and not list(msg.iter_attachments()),
            "it carries the text and an HTML copy, and no attachment", repr(kinds))
    try:
        message({**d, "items": []}, "a@example.com")
        t.check(False, "a digest with no items is refused")
    except Refused:
        t.check(True, "a digest with no items is refused")

    mst = timezone(timedelta(hours=-7))
    t.check(stamp_line("T", datetime(2026, 11, 7, 22, 2, tzinfo=mst)) == "2026-11-07 22:02 -0700 sent: T\n",
            "the stamp gives the local date and time with its offset")

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
                      ("login", "a@example.com", "good"), ("send", "Top teams pull away on CI")],
            "it logs in to Gmail over STARTTLS before it sends", repr(calls))
    calls.clear()
    try:
        send(msg, "bad", FakeSMTP)
        t.check(False, "a refused login raises and sends nothing")
    except smtplib.SMTPAuthenticationError:
        t.check(not any(c[0] == "send" for c in calls), "a refused login raises and sends nothing", repr(calls))
    return t.end()


if __name__ == "__main__":
    if sys.argv[1:] == ["--self-test"]:
        sys.exit(self_test())
    if sys.argv[1:2] in (["-h"], ["--help"]):
        print(__doc__)
        sys.exit(0)
    sys.exit(main(sys.argv[1:]))
