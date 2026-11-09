---
name: painpoints:digest
description: Email the user a ranked list of problems people asked for help with this week on Stack Exchange's life sites, with what technology could do about each
argument-hint: "(no arguments: it reads the last seven days of questions)"
allowed-tools: Read, Write, Edit, Bash(date *), Bash(.claude/scripts/stackexchange_fetch.py *), Bash(.claude/scripts/digest_mail.py *)
---

Mail the problems people are asking strangers to help them with, ranked
by how many people they reach, with what a tool could do about each.
`<date>` is `date +%F`, and every file goes in `digest/painpoints/`.

1. **Fetch.**
   ```bash
   .claude/scripts/stackexchange_fetch.py .claude/skills/painpoints:digest/sites.tsv \
     --mailed digest/painpoints --out digest/painpoints/<date>-questions.json
   ```
   No question read is a failed fetch: stop and say so.
2. **Keep the pain points.** Read `digest/painpoints/<date>-questions.json`.
   A pain
   point is a problem in someone's own life they cannot solve alone: a
   bill they cannot track, a parent they cannot reach, a landlord who
   will not answer. A question asked out of curiosity, about theory, or
   about someone else's work is not one. A question marked `mailed` is
   left out.
3. **Group and rank.** Questions describing the same underlying problem
   are one pain point, across sites. Rank by how many questions ask it,
   then by their views; at a tie, one with no accepted answer ranks
   first, since nobody has solved it.
4. **Compose** `digest/painpoints/<date>.json` (`digest_mail.py --help`):
   - **title** — the top pain point as one sentence with a verb, in the
     askers' terms.
   - **subtitle** — `Pain points from <sites> Stack Exchange sites,
     questions asked <first day> to <date>: <questions> questions read,
     <kept> pain points`.
   - **items** — up to eight pain points, ranked: a headline naming the
     problem and who has it; as source, `<n> question(s), <views> views
     on <site>…, <answered or no accepted answer>`; as what, the problem
     in two or three sentences, with one short quote of an asker; as
     why, a concrete kind of tool that would solve or ease it, naming a
     product only when an asker names it, since the read holds the
     questions and not their answers; and the most viewed question's
     link.
   - **why_label** — `What technology could do`.
   - **skip** — questions that drew many views but are not pain points,
     each `<title>: <why not>`.
   - **note** — that the questions are their askers' words on Stack
     Exchange, shared under CC BY-SA; that views count readers, not
     people with the problem; and that askers there write in English and
     already look for help online, so the people least served are the
     least heard.
5. **Send.**
   ```bash
   .claude/scripts/digest_mail.py digest/painpoints/<date>.json \
     --stamp .claude/digest/painpoints.last
   ```
   A refusal is reported as it prints, and nothing is retried.
