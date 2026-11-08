#!/usr/bin/env python3
"""feed_fetch.py — read a list of RSS, RDF or Atom feeds and print, as one
JSON document, every item published in the last N hours.

  feed_fetch.py SOURCES.tsv [--hours N] [--out FILE]   # N defaults to 24
  feed_fetch.py --self-test

SOURCES.tsv has a header line and the columns section, outlet, country,
kind and url, and optionally hours, a feed's own window in place of N; a
line starting with # is a comment. The output is

  {"fetched": "<UTC time>", "hours": N,
   "sources": [{"outlet", "section", "country", "kind", "items": <n>,
                "error": "<why it failed>" | absent}],
   "items": [{"section", "outlet", "country", "title", "link",
              "published", "summary"}]}

with items newest first and each summary cut to 300 characters of plain
text. A feed that fails is listed with its error and the rest are still
read; the exit status is 1 only when every feed failed.
"""
from __future__ import annotations

import csv
import html
import json
import re
import sys
import tempfile
import urllib.request
import xml.etree.ElementTree as ET
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timedelta, timezone
from email.utils import parsedate_to_datetime
from pathlib import Path

sys.path.insert(0, str(Path.home() / ".claude/scripts"))

AGENT = "Mozilla/5.0 (X11; Linux x86_64) digest-reader"
SUMMARY_CHARS = 300
COLUMNS = ["section", "outlet", "country", "kind", "url"]


def read_sources(path: Path) -> list[dict]:
    lines = [ln for ln in path.read_text().splitlines() if ln.strip() and not ln.startswith("#")]
    rows = list(csv.DictReader(lines, delimiter="\t"))
    if not rows or list(rows[0])[: len(COLUMNS)] != COLUMNS or len(rows[0]) > len(COLUMNS) + 1 \
            or len(rows[0]) > len(COLUMNS) and "hours" not in rows[0]:
        raise ValueError(f"{path} needs the tab-separated header {' '.join(COLUMNS)} [hours]")
    return rows


def local(tag: str) -> str:
    return tag.rsplit("}", 1)[-1]


def child_text(el: ET.Element, *names: str) -> str:
    for c in el:
        if local(c.tag) in names and (c.text or "").strip():
            return c.text.strip()
    return ""


def entry_link(el: ET.Element) -> str:
    for c in el:
        if local(c.tag) == "link":
            if c.get("href") and c.get("rel", "alternate") == "alternate":
                return c.get("href")
            if (c.text or "").strip():
                return c.text.strip()
    return ""


def when(text: str) -> datetime | None:
    if not text:
        return None
    try:
        d = parsedate_to_datetime(text)
    except (TypeError, ValueError):
        try:
            d = datetime.fromisoformat(text.replace("Z", "+00:00"))
        except ValueError:
            return None
    return d if d.tzinfo else d.replace(tzinfo=timezone.utc)


def plain(text: str) -> str:
    text = html.unescape(re.sub(r"<[^>]+>", " ", text or ""))
    text = re.sub(r"\s+", " ", text).strip()
    return text if len(text) <= SUMMARY_CHARS else text[: SUMMARY_CHARS - 1].rstrip() + "…"


def parse(xml: bytes) -> list[dict]:
    root = ET.fromstring(xml)
    out = []
    for el in root.iter():
        if local(el.tag) not in ("item", "entry"):
            continue
        out.append({
            "title": plain(child_text(el, "title")),
            "link": entry_link(el),
            "published": when(child_text(el, "pubDate", "published", "updated", "date")),
            "summary": plain(child_text(el, "description", "summary", "content")),
        })
    return out


def fetch(url: str) -> bytes:
    req = urllib.request.Request(url, headers={"User-Agent": AGENT})
    with urllib.request.urlopen(req, timeout=20) as r:
        return r.read(5_000_000)


def read_feed(src: dict, now: datetime, hours: int, get=fetch) -> tuple[dict, list[dict]]:
    report = {k: src[k] for k in ("outlet", "section", "country", "kind")}
    since = now - timedelta(hours=int(src.get("hours") or hours))
    try:
        entries = parse(get(src["url"]))
    except Exception as e:  # one feed's failure is reported, never fatal
        return {**report, "items": 0, "error": f"{type(e).__name__}: {e}"}, []
    items = [
        {"section": src["section"], "outlet": src["outlet"], "country": src["country"],
         "title": e["title"], "link": e["link"],
         "published": e["published"].astimezone(timezone.utc).strftime("%Y-%m-%dT%H:%MZ"),
         "summary": e["summary"]}
        for e in entries if e["published"] and e["published"] >= since and e["title"]
    ]
    return {**report, "items": len(items)}, items


def collect(sources: list[dict], hours: int, now: datetime, get=fetch) -> dict:
    with ThreadPoolExecutor(max_workers=8) as pool:
        results = list(pool.map(lambda s: read_feed(s, now, hours, get), sources))
    items = sorted((i for _, got in results for i in got), key=lambda i: i["published"], reverse=True)
    return {"fetched": now.strftime("%Y-%m-%dT%H:%MZ"), "hours": hours,
            "sources": [r for r, _ in results], "items": items}


def one_per_line(doc: dict) -> str:
    """The document as JSON with each source and each item on a line of its own."""
    row = lambda x: json.dumps(x, ensure_ascii=False)  # noqa: E731
    head = {k: v for k, v in doc.items() if k not in ("sources", "items")}
    return (row(head)[:-1] + ',\n"sources": [\n' + ",\n".join(map(row, doc["sources"]))
            + '],\n"items": [\n' + ",\n".join(map(row, doc["items"])) + "]}")


def main(argv: list[str]) -> int:
    args = list(argv)
    hours, out = 24, None
    if "--hours" in args:
        i = args.index("--hours"); hours = int(args[i + 1]); del args[i:i + 2]
    if "--out" in args:
        i = args.index("--out"); out = Path(args[i + 1]); del args[i:i + 2]
    if len(args) != 1:
        print(__doc__.split("\n\n")[1], file=sys.stderr)
        return 2
    try:
        doc = collect(read_sources(Path(args[0])), hours, datetime.now(timezone.utc))
    except (OSError, ValueError) as e:
        print(f"feed_fetch.py: {e}", file=sys.stderr)
        return 1
    text = one_per_line(doc)
    if out:
        out.write_text(text + "\n")
    else:
        print(text)
    failed = [s for s in doc["sources"] if "error" in s]
    for s in failed:
        print(f"feed_fetch.py: {s['outlet']}: {s['error']}", file=sys.stderr)
    print(f"feed_fetch.py: {len(doc['items'])} items from {len(doc['sources']) - len(failed)}"
          f" of {len(doc['sources'])} feeds", file=sys.stderr)
    return 1 if len(failed) == len(doc["sources"]) else 0


RSS = b"""<?xml version="1.0"?><rss version="2.0"><channel><title>T</title>
<item><title>Quake hits &lt;b&gt;coast&lt;/b&gt;</title><link>https://a.example/1</link>
<pubDate>Sun, 08 Nov 2026 10:00:00 +0000</pubDate><description>&lt;p&gt;Many  hurt&lt;/p&gt;</description></item>
<item><title>Old story</title><link>https://a.example/0</link><pubDate>Thu, 05 Nov 2026 10:00:00 +0000</pubDate></item>
</channel></rss>"""
ATOM = b"""<?xml version="1.0"?><feed xmlns="http://www.w3.org/2005/Atom"><title>T</title>
<entry><title>Chip ships</title><link rel="alternate" href="https://b.example/2"/>
<updated>2026-11-08T12:00:00Z</updated><summary>Fast.</summary></entry></feed>"""
RDF = b"""<?xml version="1.0"?><rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
xmlns="http://purl.org/rss/1.0/" xmlns:dc="http://purl.org/dc/elements/1.1/">
<item><title>Vote held</title><link>https://c.example/3</link><dc:date>2026-11-08T08:00:00+01:00</dc:date></item></rdf:RDF>"""


def self_test() -> int:
    from tap import Tap
    t = Tap()
    now = datetime(2026, 11, 8, 14, 0, tzinfo=timezone.utc)
    feeds = {"https://a/rss": RSS, "https://b/atom": ATOM, "https://c/rdf": RDF}

    def get(url: str) -> bytes:
        if url not in feeds:
            raise OSError("HTTP Error 404")
        return feeds[url]

    src = lambda o, u: {"section": "world", "outlet": o, "country": "UK", "kind": "public", "url": u}  # noqa: E731
    doc = collect([src("A", "https://a/rss"), src("B", "https://b/atom"), src("C", "https://c/rdf"),
                   src("D", "https://d/gone")], 24, now, get)
    wide = collect([{**src("A", "https://a/rss"), "hours": "96"}], 24, now, get)
    t.check([i["title"] for i in wide["items"]] == ["Quake hits coast", "Old story"],
            "a feed's own hours widens its window", repr(wide["items"]))
    titles = [i["title"] for i in doc["items"]]
    t.check(titles == ["Chip ships", "Quake hits coast", "Vote held"],
            "RSS, Atom and RDF items in the window are kept, newest first", repr(titles))
    t.check("Old story" not in titles, "an item older than the window is dropped")
    quake = doc["items"][1]
    t.check(quake["summary"] == "Many hurt" and quake["link"] == "https://a.example/1",
            "markup is stripped to plain text and the link kept", repr(quake))
    t.check(doc["items"][2]["published"] == "2026-11-08T07:00Z", "times are given in UTC")
    text = one_per_line(doc)
    t.check(json.loads(text) == doc and len(text.splitlines()) == 3 + len(doc["sources"]) + len(doc["items"]),
            "the output is the document, one source or item per line", text)
    gone = [s for s in doc["sources"] if s["outlet"] == "D"][0]
    t.check(gone["items"] == 0 and "404" in gone["error"], "a failed feed is listed with its error", repr(gone))
    t.check(plain("x" * 400).endswith("…") and len(plain("x" * 400)) == SUMMARY_CHARS,
            "a long summary is cut to its limit")
    with tempfile.TemporaryDirectory() as tmp:
        bad = Path(tmp) / "s.tsv"
        bad.write_text("outlet\turl\nA\thttps://a/rss\n")
        try:
            read_sources(bad)
            t.check(False, "a sources file without the header is refused")
        except ValueError:
            t.check(True, "a sources file without the header is refused")
        good = Path(tmp) / "g.tsv"
        good.write_text("# comment\n" + "\t".join(COLUMNS) + "\thours\nworld\tA\tUK\tpublic\thttps://a/rss\t\n")
        t.check(read_sources(good)[0]["outlet"] == "A", "comments are skipped and rows read by header")
    return t.end()


if __name__ == "__main__":
    if sys.argv[1:] == ["--self-test"]:
        sys.exit(self_test())
    if sys.argv[1:2] in (["-h"], ["--help"]):
        print(__doc__)
        sys.exit(0)
    sys.exit(main(sys.argv[1:]))
