#!/usr/bin/env python3
"""stackexchange_fetch.py — read the questions asked in the last N days on a
list of Stack Exchange sites, through the public API with no key (about
300 requests a day; one request per site), and print them as one JSON
document.

  stackexchange_fetch.py SITES.tsv [--days N] [--mailed DIR] [--out FILE]
  stackexchange_fetch.py --self-test

SITES.tsv has a header line and the columns site (the API name, such as
`parenting`) and area (what part of life it covers); a line starting with
# is a comment. N defaults to 7. --mailed DIR marks a question `mailed`
when its link is an item's link in a digest JSON in DIR from the last N
days, so a problem already sent is not sent again. The output is

  {"fetched": "<UTC time>", "days": N, "quota_remaining": <n>,
   "sites": [{"site", "area", "questions": <n>, "error": absent | "<why>"}],
   "questions": [{"site", "area", "title", "link", "views", "score",
                  "answers", "accepted", "asked", "tags", "body", "mailed"}]}

one site or question per line, questions with the most views first, each
body cut to 400 characters of plain text. A site that fails is listed with
its error; the exit status is 1 only when every site failed.
"""
from __future__ import annotations

import csv
import gzip
import html
import json
import re
import sys
import tempfile
import urllib.parse
import urllib.request
from datetime import datetime, timedelta, timezone
from pathlib import Path

sys.path.insert(0, str(Path.home() / ".claude/scripts"))

API = "https://api.stackexchange.com/2.3/questions"
AGENT = "Mozilla/5.0 (X11; Linux x86_64) digest-reader"
BODY_CHARS = 400
COLUMNS = ["site", "area"]


def read_sites(path: Path) -> list[dict]:
    lines = [ln for ln in path.read_text().splitlines() if ln.strip() and not ln.startswith("#")]
    rows = list(csv.DictReader(lines, delimiter="\t"))
    if not rows or list(rows[0]) != COLUMNS:
        raise ValueError(f"{path} needs the tab-separated header {' '.join(COLUMNS)}")
    return rows


def plain(text: str) -> str:
    text = html.unescape(re.sub(r"<[^>]+>", " ", text or ""))
    text = re.sub(r"\s+([.,;:!?])", r"\1", re.sub(r"\s+", " ", text)).strip()
    return text if len(text) <= BODY_CHARS else text[: BODY_CHARS - 1].rstrip() + "…"


def get_json(url: str) -> dict:
    req = urllib.request.Request(url, headers={"User-Agent": AGENT, "Accept-Encoding": "gzip"})
    with urllib.request.urlopen(req, timeout=30) as r:
        raw = r.read(10_000_000)
    return json.loads(gzip.decompress(raw) if raw[:2] == b"\x1f\x8b" else raw)


def site_url(site: str, since: datetime) -> str:
    q = {"site": site, "fromdate": int(since.timestamp()), "sort": "votes", "order": "desc",
         "pagesize": 100, "filter": "withbody"}
    return f"{API}?{urllib.parse.urlencode(q)}"


def question(row: dict, q: dict, mailed: set[str]) -> dict:
    link = q.get("link", "")
    return {"site": row["site"], "area": row["area"], "title": html.unescape(q.get("title", "")),
            "link": link, "views": q.get("view_count", 0), "score": q.get("score", 0),
            "answers": q.get("answer_count", 0), "accepted": "accepted_answer_id" in q,
            "asked": datetime.fromtimestamp(q.get("creation_date", 0), timezone.utc).strftime("%Y-%m-%d"),
            "tags": q.get("tags", []), "body": plain(q.get("body", "")), "mailed": link in mailed}


def mailed_links(folder: Path | None, now: datetime, days: int) -> set[str]:
    if not folder or not folder.is_dir():
        return set()
    cutoff = (now - timedelta(days=days)).strftime("%Y-%m-%d")
    links = set()
    for f in folder.glob("????-??-??.json"):
        if f.stem < cutoff:
            continue
        try:
            links |= {it.get("link", "") for it in json.loads(f.read_text()).get("items", [])}
        except (OSError, ValueError):
            continue
    return links - {""}


def collect(rows: list[dict], days: int, now: datetime, mailed: set[str], get=get_json) -> dict:
    since = now - timedelta(days=days)
    sites, questions, quota = [], [], None
    for row in rows:
        try:
            doc = get(site_url(row["site"], since))
        except Exception as e:  # one site's failure is reported, never fatal
            sites.append({**row, "questions": 0, "error": f"{type(e).__name__}: {e}"})
            continue
        if "error_message" in doc:
            sites.append({**row, "questions": 0, "error": doc["error_message"]})
            continue
        quota = doc.get("quota_remaining", quota)
        got = [question(row, q, mailed) for q in doc.get("items", [])]
        sites.append({**row, "questions": len(got)})
        questions += got
    questions.sort(key=lambda q: q["views"], reverse=True)
    return {"fetched": now.strftime("%Y-%m-%dT%H:%MZ"), "days": days, "quota_remaining": quota,
            "sites": sites, "questions": questions}


def one_per_line(doc: dict) -> str:
    row = lambda x: json.dumps(x, ensure_ascii=False)  # noqa: E731
    head = {k: v for k, v in doc.items() if k not in ("sites", "questions")}
    return (row(head)[:-1] + ',\n"sites": [\n' + ",\n".join(map(row, doc["sites"]))
            + '],\n"questions": [\n' + ",\n".join(map(row, doc["questions"])) + "]}")


def main(argv: list[str]) -> int:
    args = list(argv)
    opts = {"--days": "7", "--mailed": None, "--out": None}
    for k in opts:
        if k in args:
            i = args.index(k); opts[k] = args[i + 1]; del args[i:i + 2]
    if len(args) != 1:
        print(__doc__.split("\n\n")[1], file=sys.stderr)
        return 2
    now, days = datetime.now(timezone.utc), int(opts["--days"])
    try:
        mailed = mailed_links(Path(opts["--mailed"]) if opts["--mailed"] else None, now, days)
        doc = collect(read_sites(Path(args[0])), days, now, mailed)
    except (OSError, ValueError) as e:
        print(f"stackexchange_fetch.py: {e}", file=sys.stderr)
        return 1
    text = one_per_line(doc)
    if opts["--out"]:
        Path(opts["--out"]).write_text(text + "\n")
    else:
        print(text)
    failed = [s for s in doc["sites"] if "error" in s]
    for s in failed:
        print(f"stackexchange_fetch.py: {s['site']}: {s['error']}", file=sys.stderr)
    print(f"stackexchange_fetch.py: {len(doc['questions'])} questions from {len(doc['sites']) - len(failed)}"
          f" of {len(doc['sites'])} sites; quota left {doc['quota_remaining']}", file=sys.stderr)
    return 1 if len(failed) == len(doc["sites"]) else 0


def self_test() -> int:
    from tap import Tap
    t = Tap()
    now = datetime(2026, 11, 8, 14, 0, tzinfo=timezone.utc)
    seen = []

    def get(url: str) -> dict:
        seen.append(url)
        if "site=gone" in url:
            raise OSError("HTTP Error 400")
        if "site=throttled" in url:
            return {"error_id": 502, "error_message": "too many requests from this IP"}
        views = 900 if "site=money" in url else 40
        return {"quota_remaining": 280, "items": [
            {"title": "Can&#39;t track &amp; split bills", "link": f"https://x/{views}", "view_count": views,
             "score": 3, "answer_count": 0, "creation_date": 1762500000, "tags": ["budget"],
             "body": "<p>Every  month I lose <b>receipts</b>.</p>"}]}

    rows = [{"site": "parenting", "area": "family"}, {"site": "money", "area": "money"},
            {"site": "gone", "area": "x"}, {"site": "throttled", "area": "y"}]
    doc = collect(rows, 7, now, {"https://x/40"}, get)
    t.check([q["site"] for q in doc["questions"]] == ["money", "parenting"],
            "questions from every site are kept, most viewed first", repr(doc["questions"]))
    q = doc["questions"][0]
    t.check(q["title"] == "Can't track & split bills" and q["body"] == "Every month I lose receipts.",
            "titles are unescaped and bodies made plain text", repr(q))
    t.check(q["accepted"] is False and q["answers"] == 0 and q["asked"] == "2025-11-07",
            "a question carries its answers, whether one is accepted, and the day it was asked", repr(q))
    t.check(doc["questions"][1]["mailed"] and not q["mailed"], "a link already mailed is marked mailed")
    errs = {s["site"]: s.get("error", "") for s in doc["sites"]}
    t.check("400" in errs["gone"] and "too many" in errs["throttled"] and not errs["money"],
            "a failed or refused site is listed with its error", repr(errs))
    t.check("fromdate=1793541600" in seen[0] and "sort=votes" in seen[0] and "filter=withbody" in seen[0],
            "each site is asked for the window's questions with their bodies", seen[0])
    text = one_per_line(doc)
    t.check(json.loads(text) == doc and len(text.splitlines()) == 3 + len(doc["sites"]) + len(doc["questions"]),
            "the output is the document, one site or question per line", text)
    with tempfile.TemporaryDirectory() as tmp:
        d = Path(tmp)
        (d / "2026-11-07.json").write_text(json.dumps({"items": [{"link": "https://x/1"}, {"link": ""}]}))
        (d / "2026-10-01.json").write_text(json.dumps({"items": [{"link": "https://x/old"}]}))
        (d / "2026-11-07-questions.json").write_text("{}")
        t.check(mailed_links(d, now, 7) == {"https://x/1"},
                "only digests inside the window count as mailed", repr(mailed_links(d, now, 7)))
        bad = d / "s.tsv"
        bad.write_text("name\tarea\nparenting\tfamily\n")
        try:
            read_sites(bad)
            t.check(False, "a sites file without the header is refused")
        except ValueError:
            t.check(True, "a sites file without the header is refused")
    return t.end()


if __name__ == "__main__":
    if sys.argv[1:] == ["--self-test"]:
        sys.exit(self_test())
    if sys.argv[1:2] in (["-h"], ["--help"]):
        print(__doc__)
        sys.exit(0)
    sys.exit(main(sys.argv[1:]))
