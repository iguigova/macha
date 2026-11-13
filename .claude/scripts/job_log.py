#!/usr/bin/env python3
"""job_log.py — what the job log says each source of postings led to: per
source, the jobs found there, applied to, and replied to, by kind of reply.

  job_log.py [HISTORY]      # default .claude/session_history.md
  job_log.py --self-test

A job is every journal entry under one `job: <Company> — <Role>` topic. Its
source is the first bullet text "found on <source>" up to a ";" or the
line's end; its application is the closing of the note an entry under that topic
opened, wherever that closing sits, whose text starts "applied";
its reply is the last bullet that starts "Reply: <kind>", the kind being
the word before " — " or the line's end. A job with no source counts under
"(not recorded)". Prints one TSV row per source, the most applied, then the most replied, first:

  source  found  applied  replied  <kind>=<n>…
"""
from __future__ import annotations

import re
import sys
import tempfile
from collections import defaultdict
from pathlib import Path

sys.path.insert(0, str(Path.home() / ".claude/scripts"))

HEAD = re.compile(r"^## \S+ \S+ \[\w+\] — job: (.+?)(?: \(via [^)]*\))?$")
FOUND = re.compile(r"found on ([^;\n]+)", re.I)
REPLY = re.compile(r"^- Reply: (\S[^—\n]*?)(?: — |$)")
NOTE = re.compile(r"^\*\*TODO:\*\* \[(\w+)\] ")
DONE = re.compile(r"^\*\*TODO-DONE:\*\* \[(\w+)\] \S+ — (.*)$")


def jobs(text: str) -> dict[str, dict]:
    out: dict[str, dict] = defaultdict(lambda: {"source": None, "applied": False, "reply": None})
    note_topic: dict[str, str] = {}
    done: dict[str, str] = {}
    topic = None
    for line in text.splitlines():
        m = HEAD.match(line)
        if m:
            topic = m.group(1); out[topic]; continue
        if line.startswith("## "):
            topic = None
        if d := DONE.match(line):
            done[d.group(1)] = d.group(2)
        if topic is None:
            continue
        j = out[topic]
        if n := NOTE.match(line):
            note_topic[n.group(1)] = topic
        if j["source"] is None and line.startswith("- ") and (f := FOUND.search(line)):
            j["source"] = f.group(1).strip()
        if r := REPLY.match(line):
            j["reply"] = r.group(1).strip().lower()
    for nid, topic in note_topic.items():
        if done.get(nid, "").lower().startswith("applied"):
            out[topic]["applied"] = True
    return out


def tally(js: dict[str, dict]) -> list[str]:
    rows: dict[str, dict] = defaultdict(lambda: {"found": 0, "applied": 0, "kinds": defaultdict(int)})
    for j in js.values():
        r = rows[j["source"] or "(not recorded)"]
        r["found"] += 1
        r["applied"] += j["applied"]
        if j["reply"]:
            r["kinds"][j["reply"]] += 1
    lines = []
    for src, r in sorted(rows.items(), key=lambda kv: (-kv[1]["applied"], -sum(kv[1]["kinds"].values()), -kv[1]["found"], kv[0])):
        kinds = " ".join(f"{k}={n}" for k, n in sorted(r["kinds"].items()))
        lines.append("\t".join([src, str(r["found"]), str(r["applied"]), str(sum(r["kinds"].values()))]
                               + ([kinds] if kinds else [])))
    return lines


def self_test() -> int:
    from tap import Tap
    t = Tap()
    hist = """## 2026-11-12 22:02 [aaaa] — job: A — Eng (via /job:apply)

- https://a/1 — found on HN Who's Hiring; remote Canada, $1; fits Go
**TODO:** [x1] A — Eng: https://a/1

## 2026-11-13 10:00 [bbbb] — job: A — Eng (via /job:apply)

- Applied via Ashby

## 2026-11-20 10:00 [cccc] — job: A — Eng (via /job:apply)

- Reply: interview — a call with the CTO

## 2026-11-14 09:00 [gggg] — chore: batch close

**TODO-DONE:** [x1] - — applied 2026-11-13 on Ashby

## 2026-11-12 22:03 [dddd] — job: B — Dev (via /job:apply)

- https://b/1 — found on LinkedIn; remote Canada
**TODO:** [x2] B — Dev: https://b/1
**TODO-DONE:** [x2] - — skipped by the user

## 2026-11-12 22:04 [eeee] — job: C — Dev (via /job:apply)

- https://c/1 — remote Canada; fits Go
**TODO:** [x3] C — Dev: https://c/1
**TODO-DONE:** [x3] - — applied by email

## 2026-11-12 22:05 [ffff] — chore: unrelated

- found on nowhere
"""
    js = jobs(hist)
    t.check(set(js) == {"A — Eng", "B — Dev", "C — Dev"}, "only job topics are jobs", str(set(js)))
    t.check(js["A — Eng"] == {"source": "HN Who's Hiring", "applied": True, "reply": "interview"},
            "a job's entries, and its note closed under another topic, join into one", str(js["A — Eng"]))
    t.check(not js["B — Dev"]["applied"], "a skipped job is not applied", str(js["B — Dev"]))
    rows = tally(js)
    t.check(rows == ["HN Who's Hiring\t1\t1\t1\tinterview=1", "(not recorded)\t1\t1\t0", "LinkedIn\t1\t0\t0"],
            "rows tally each source, the most applied first", "\n".join(rows))
    with tempfile.NamedTemporaryFile("w", suffix=".md", delete=False) as f:
        f.write(hist)
    import contextlib, io
    with contextlib.redirect_stdout(io.StringIO()) as buf:
        st = main([f.name])
    t.check(st == 0 and buf.getvalue().startswith("source\tfound"), "a history file reads and prints", buf.getvalue())
    return t.end()


def main(argv: list[str]) -> int:
    if argv and argv[0] in ("-h", "--help"):
        print(__doc__.strip()); return 0
    if argv and argv[0] == "--self-test":
        return self_test()
    path = Path(argv[0] if argv else ".claude/session_history.md")
    print("source\tfound\tapplied\treplied\tkinds")
    for row in tally(jobs(path.read_text())):
        print(row)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
