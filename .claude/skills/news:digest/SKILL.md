---
name: news:digest
description: Email the user the day's top world, tech and science news, ranked by how widely independent outlets in different countries cover each story, with every claim attributed
argument-hint: "(no arguments: it reads the feeds as they are now)"
allowed-tools: Read, Write, Edit, WebFetch, Bash(date *), Bash(.claude/scripts/feed_fetch.py *), Bash(.claude/scripts/digest_mail.py *)
---

Mail the day's world, tech and science news, chosen by how widely it is
covered rather than by any one outlet's choice. `<date>` is `date +%F`,
and every file goes in `digest/news/`.

1. **Fetch.**
   ```bash
   .claude/scripts/feed_fetch.py .claude/skills/news:digest/sources.tsv \
     --out digest/news/<date>-feeds.json
   ```
   Fewer than half the feeds read is a failed fetch: stop and say so.
2. **Group.** Read `digest/news/<date>-feeds.json`. A story is one event: items
   from different outlets about the same event are one story. For each
   story count its outlets and their countries, within its section.
3. **Rank.** Within each section, the stories most widely covered come
   first: more outlets, then more countries. A story only one outlet
   carries is never an item, whatever its headline claims, except in
   Science: there a story one outlet carries is an item when WebFetch
   finds the peer-reviewed paper it reports, ranked after the stories
   two or more outlets carry, at most six such articles fetched. The
   outlets' own headlines, their order and their adjectives decide
   nothing. A section with fewer qualifying stories than its limit holds
   only those.
4. **Write each item from what the outlets agree on.**
   - **headline** — what happened, in plain verbs: who did what, where.
     No word that judges, such as "slams", "historic", "shocking" or
     "controversial", unless it is a quote attributed to its speaker.
   - **source** — `Covered by <n> of <total> outlets in <c> countries:
     <outlet>, <outlet>, …`.
   - **what** — two or three sentences of facts at least two outlets
     report, or, for a science story one outlet carries, facts its
     paper's abstract states. A figure, a cause or a blame stated by one side is
     attributed to it: "Ukraine's army says…", "the company says…".
     Where outlets report a fact differently, or one leaves out what the
     others lead with, say so in one sentence naming them.
   - **why** — who it affects and how, from the facts reported.
   - **link** — one outlet's article: a public broadcaster's when one
     covers the story. For a science story, use WebFetch on that article
     to find the paper it reports and link the paper itself, saying in
     **what** whether it is peer-reviewed or a preprint; a story whose
     paper cannot be found keeps the article's link and says so.
   - **section** — `World`, `Tech` or `Science`.
5. **Compose** `digest/news/<date>.json` (`digest_mail.py --help`):
   - **title** — the most widely covered story, as one sentence with a
     verb, in the same plain words as its headline.
   - **subtitle** — `News for <date>, read at <time> from <read> of
     <total> feeds: <stories> stories, <items> items`.
   - **items** — up to five World, then up to three Tech, then up to
     three Science stories, each section ranked as in step 3.
   - **why_label** — `Who it affects`.
   - **skip** — stories that dominated one outlet's feed or one
     country's outlets but no one else's, each `<outlet>: <story>`.
   - **note** — that the ranking counts outlets, not truth; that the
     outlets are the ones `sources.tsv` lists, with how many of them
     each country has; and which feeds failed, if any.
6. **Send.**
   ```bash
   .claude/scripts/digest_mail.py digest/news/<date>.json \
     --stamp .claude/digest/news.last
   ```
   A refusal is reported as it prints, and nothing is retried.
