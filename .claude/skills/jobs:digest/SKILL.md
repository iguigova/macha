---
name: jobs:digest
description: Email the user the new postings at niche companies, found anew each morning by web search, that fit the profile, the day they appear, before the aggregators carry them
argument-hint: "(no arguments: it finds the companies anew each run)"
allowed-tools: Read, Write, Edit, Bash(date *), Bash(curl *), Bash(jq *), WebSearch, Bash(.claude/scripts/job_board.sh *), Bash(.claude/scripts/board_watch.py *), Bash(.claude/scripts/digest_mail.py *), Bash(~/.claude/scripts/journal_queue.sh *)
---

Mail the postings that appeared at niche companies since the last send
and fit the profile, ranked, so the user applies while few others have.
`<date>` is `date +%F`, and every file goes in `digest/jobs/`.

1. **Find the companies**, anew each run and never from a kept list.
   Read `jobs/profile/profile.txt` in full. For each of its "I want" and
   "I am interested in" lines, a `WebSearch` for the companies building
   open-source projects, products or unusual problems in that area,
   which aggregators rarely carry. Find each company's board:
   ```bash
   .claude/scripts/job_board.sh <greenhouse|ashby|lever> <board>
   ```
   An exit 4 means the board is under another ATS or name. Write each
   board that answers as a row of `digest/jobs/<date>-boards.tsv`, under
   the header `ats	board	company	area`, its area the interest it
   serves.
2. **Fetch.**
   ```bash
   .claude/scripts/board_watch.py fetch digest/jobs/<date>-boards.tsv \
     --seen digest/jobs/seen.tsv --out digest/jobs/<date>-postings.json
   ```
   No board found, or an exit 1, every board failed, is a failed fetch:
   stop and say so.
3. **Keep the ones that fit.** Read `digest/jobs/<date>-postings.json`.
   Of the postings marked `new`, drop
   each whose url a job note carries:
   ```bash
   ~/.claude/scripts/journal_queue.sh --topic "job: "
   ~/.claude/scripts/journal_queue.sh --closed --topic "job: "
   ```
   Drop each whose location rule plainly excludes where the user may
   work. Read each other one whole from its ATS's API, the line for its
   ats:
   ```bash
   curl -sS 'https://boards-api.greenhouse.io/v1/boards/<board>/jobs/<id>?content=true' | jq -r .content
   curl -sS 'https://api.ashbyhq.com/posting-api/job-board/<board>?includeCompensation=true' | jq -r '.jobs[] | select(.id == "<id>") | .descriptionPlain'
   curl -sS 'https://api.lever.co/v0/postings/<board>/<id>' | jq -r '.descriptionPlain, (.lists[] | .text, .content)'
   ```

   Keep it when it fits as `/job:apply`'s Check at source states, from
   profile facts only.
4. **Rank** as `/job:apply`'s Shortlist states.
5. **Compose** `digest/jobs/<date>.json` (`digest_mail.py --help`):
   - **title** — the top posting as one sentence with a verb, naming the
     company and the work, such as `Tailscale is hiring a security
     engineer, remote from Canada`; with none kept, `No new niche
     posting fits today`.
   - **subtitle** — `<boards> niche companies found and read <date>:
     <new> new postings, <kept> fit`, and each board that failed, by
     name.
   - **items** — every kept posting, ranked: as headline `<Company> —
     <Role>`; as source `<area>; first posted <posted>; <location rule>;
     <pay or "pay not stated">`; as what, the work in two sentences in
     the posting's terms; as why, the profile facts it fits and each gap,
     quoting the profile's words; and its url as link.
   - **why_label** — `Why it fits`.
   - **skip** — each new posting dropped, `<Company> — <Role>: <why
     not>`, the reason in a few words.
   - **note** — that `/job:apply <url>` queues and applies to one, and
     that the companies were found by this morning's web search, from the
     profile's interests.
6. **Send, then mark.**
   ```bash
   .claude/scripts/digest_mail.py digest/jobs/<date>.json --stamp .claude/digest/jobs.last \
     && .claude/scripts/board_watch.py mark digest/jobs/<date>-postings.json --seen digest/jobs/seen.tsv
   ```
   A refusal is reported as it prints, and nothing is retried; the
   postings stay new for the next run.
