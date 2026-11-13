#!/usr/bin/env python3
"""board_watch.py — list the engineering postings on a set of company boards,
through job_board.sh, and say which are new, as one JSON document.

  board_watch.py fetch BOARDS.tsv --seen FILE [--days N] [--out FILE]
  board_watch.py mark OUT.json --seen FILE
  board_watch.py --self-test

BOARDS.tsv has a header line and the columns ats, board, company and area;
a line starting with # is a comment. `fetch` lists every board and keeps
the postings whose title names engineering work (engineer, developer,
programmer, architect) and not sales, support or management. A posting is
`new` when its url is not in the --seen file and it was first posted in
the last N days (default 14), or its board states no date. The output is

  {"fetched": "<UTC time>", "days": N,
   "boards": [{"ats", "board", "company", "area", "postings": <n>,
               "error": absent | "<why>"}],
   "postings": [{"company", "area", "ats", "board", "id", "title",
                 "location", "workplace", "pay", "url", "posted", "new"}]}

one board or posting per line, newest posted first. A board that fails is
listed with its error; the exit status is 1 only when every board failed.
`mark` appends the url of every posting OUT.json lists to the --seen file,
one per line with the day, so a posting is new once: a digest runs it after
its send, and a failed send leaves the postings new for the next run.
JOB_BOARD names the board reader, job_board.sh beside this script by
default.
"""
from __future__ import annotations

import csv
import json
import os
import re
import subprocess
import sys
import tempfile
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

sys.path.insert(0, str(Path.home() / ".claude/scripts"))

HERE = Path(__file__).resolve().parent
WORK = re.compile(r"engineer|developer|programmer|architect", re.I)
NOT_WORK = re.compile(r"sales|account|support|success|solutions|manager|director|"
                      r"head of|recruit|marketing|designer|intern|deployment strategist", re.I)
EXITS = {1: "closed", 2: "usage", 3: "unreachable or another HTTP answer", 4: "no such board"}


def read_boards(path: Path) -> list[dict]:
    lines = [l for l in path.read_text().splitlines() if l.strip() and not l.startswith("#")]
    return list(csv.DictReader(lines, delimiter="\t"))


def read_seen(path: Path) -> set[str]:
    if not path.exists():
        return set()
    return {l.split("\t")[0] for l in path.read_text().splitlines() if l.strip()}


def list_board(ats: str, board: str) -> tuple[list[list[str]], str | None]:
    reader = os.environ.get("JOB_BOARD", str(HERE / "job_board.sh"))
    r = subprocess.run([reader, ats, board], capture_output=True, text=True)
    if r.returncode != 0:
        return [], (r.stderr.strip() or EXITS.get(r.returncode, f"exit {r.returncode}"))
    return [l.split("\t") for l in r.stdout.splitlines() if l.strip()], None


def fetch(boards: list[dict], seen: set[str], days: int, today: date) -> tuple[dict, int]:
    since = (today - timedelta(days=days)).isoformat()
    out_boards, postings = [], []
    for b in boards:
        rows, err = list_board(b["ats"], b["board"])
        entry = {k: b[k] for k in ("ats", "board", "company", "area")}
        kept = 0
        for row in rows:
            row += [""] * (7 - len(row))
            pid, title, loc, work, pay, url, posted = row[:7]
            if not WORK.search(title) or NOT_WORK.search(title):
                continue
            kept += 1
            postings.append({**entry, "id": pid, "title": title, "location": loc,
                             "workplace": work, "pay": pay, "url": url, "posted": posted,
                             "new": url not in seen and (not posted or posted >= since)})
        entry["postings"] = kept
        if err:
            entry["error"] = err
        out_boards.append(entry)
    postings.sort(key=lambda p: p["posted"], reverse=True)
    doc = {"fetched": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"), "days": days,
           "boards": out_boards, "postings": postings}
    failed = bool(boards) and all("error" in b for b in out_boards)
    return doc, 1 if failed else 0


def dump(doc: dict) -> str:
    def lines(key):
        return ",\n".join("  " + json.dumps(x, ensure_ascii=False) for x in doc[key])
    head = {k: v for k, v in doc.items() if k not in ("boards", "postings")}
    return (json.dumps(head, ensure_ascii=False)[:-1]
            + ',\n "boards": [\n' + lines("boards") + '\n ],\n "postings": [\n' + lines("postings") + "\n ]}\n")


def mark(out: Path, seen: Path, today: date) -> int:
    doc = json.loads(out.read_text())
    have = read_seen(seen)
    new = [p["url"] for p in doc["postings"] if p["url"] not in have]
    seen.parent.mkdir(parents=True, exist_ok=True)
    with seen.open("a") as f:
        for u in dict.fromkeys(new):
            f.write(f"{u}\t{today.isoformat()}\n")
    print(f"marked {len(dict.fromkeys(new))} postings seen")
    return 0


def arg(argv: list[str], flag: str, default: str | None = None) -> str | None:
    return argv[argv.index(flag) + 1] if flag in argv and argv.index(flag) + 1 < len(argv) else default


def main(argv: list[str]) -> int:
    if not argv or argv[0] in ("-h", "--help"):
        print(__doc__.strip()); return 0 if argv else 2
    if argv[0] == "--self-test":
        return self_test()
    seen = arg(argv, "--seen")
    if len(argv) < 2 or not seen:
        print(__doc__.strip(), file=sys.stderr); return 2
    if argv[0] == "mark":
        return mark(Path(argv[1]), Path(seen), date.today())
    if argv[0] != "fetch":
        print(__doc__.strip(), file=sys.stderr); return 2
    doc, st = fetch(read_boards(Path(argv[1])), read_seen(Path(seen)), int(arg(argv, "--days", "14")), date.today())
    text = dump(doc)
    out = arg(argv, "--out")
    if out:
        Path(out).parent.mkdir(parents=True, exist_ok=True); Path(out).write_text(text)
        print(f"{sum(p['new'] for p in doc['postings'])} new of {len(doc['postings'])} postings on "
              f"{len(doc['boards'])} boards, {sum('error' in b for b in doc['boards'])} failed: {out}")
    else:
        sys.stdout.write(text)
    return st


def self_test() -> int:
    from tap import Tap
    t = Tap()
    with tempfile.TemporaryDirectory() as d:
        d = Path(d)
        fake = d / "board.sh"
        fake.write_text("#!/usr/bin/env bash\ncase \"$2\" in\n"
                        "  good) printf 'a\\tSenior Go Engineer\\tRemote - Canada\\tRemote\\t\\thttps://x/a\\t2026-11-10\\n"
                        "b\\tAccount Executive\\tRemote\\t\\t\\thttps://x/b\\t2026-11-10\\n"
                        "c\\tSales Engineer\\tRemote\\t\\t\\thttps://x/c\\t2026-11-10\\n"
                        "d\\tBackend Developer\\tToronto\\t\\t\\thttps://x/d\\t2026-01-02\\n"
                        "e\\tStaff Engineer\\tRemote\\t\\t\\thttps://x/e\\t\\n"
                        "f\\tPlatform Engineer\\tRemote\\t\\t\\thttps://x/f\\t2026-11-11\\n' ;;\n"
                        "  *) echo 'no board: x' >&2; exit 4 ;;\nesac\n")
        fake.chmod(0o755)
        os.environ["JOB_BOARD"] = str(fake)
        boards = d / "boards.tsv"
        boards.write_text("# comment\nats\tboard\tcompany\tarea\nashby\tgood\tGood\tnetworking\nashby\tgone\tGone\tx\n")
        seen = d / "seen.tsv"
        seen.write_text("https://x/f\t2026-11-11\n")
        doc, st = fetch(read_boards(boards), read_seen(seen), 14, date(2026, 11, 12))
        urls = [p["url"] for p in doc["postings"]]
        t.check(st == 0, "a run where one board answers exits 0", str(st))
        t.check(urls == ["https://x/f", "https://x/a", "https://x/d", "https://x/e"],
                "it keeps engineering titles, newest posted first", str(urls))
        t.check("https://x/b" not in urls and "https://x/c" not in urls,
                "sales titles are dropped, a Sales Engineer too", str(urls))
        new = {p["url"]: p["new"] for p in doc["postings"]}
        t.check(new["https://x/a"], "a fresh posting not seen is new", str(new))
        t.check(not new["https://x/f"], "a posting in the seen file is not new", str(new))
        t.check(not new["https://x/d"], "a posting older than the window is not new", str(new))
        t.check(new["https://x/e"], "a posting with no date is new until seen", str(new))
        gone = [b for b in doc["boards"] if b["board"] == "gone"][0]
        t.check(gone.get("error") == "no board: x", "a failed board is listed with its error", str(gone))
        t.check(json.loads(dump(doc)) == doc, "the output is JSON that reads back whole")
        _, st = fetch([{"ats": "ashby", "board": "gone", "company": "G", "area": "x"}], set(), 14, date(2026, 11, 12))
        t.check(st == 1, "a run where every board failed exits 1", str(st))
        out = d / "out.json"
        out.write_text(dump(doc))
        import contextlib, io
        with contextlib.redirect_stdout(io.StringIO()):
            mark(out, seen, date(2026, 11, 12))
            mark(out, seen, date(2026, 11, 12))
        lines = seen.read_text().splitlines()
        t.check(len(lines) == 4 and lines[1] == "https://x/a\t2026-11-12",
                "mark appends each listed url once, with the day", "\n".join(lines))
        doc2, _ = fetch(read_boards(boards), read_seen(seen), 14, date(2026, 11, 12))
        t.check(not any(p["new"] for p in doc2["postings"]), "after mark nothing is new")
    return t.end()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
