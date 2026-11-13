---
name: jobs:digest
description: Email the user the new postings on niche company boards that fit the profile, the day they appear, before the aggregators carry them
argument-hint: "(no arguments: it reads every board in boards.tsv)"
allowed-tools: Read, Write, Edit, Bash(date *), Bash(curl *), Bash(jq *), Bash(.claude/scripts/board_watch.py *), Bash(.claude/scripts/digest_mail.py *), Bash(~/.claude/scripts/journal_queue.sh *)
---

Mail the postings that appeared on the niche company boards since the
last send and fit the profile, ranked, so the user applies while few
others have. `<date>` is `date +%F`, and every file goes in
`digest/jobs/`.

1. **Fetch.**
   ```bash
   .claude/scripts/board_watch.py fetch .claude/skills/jobs:digest/boards.tsv \
     --seen digest/jobs/seen.tsv --out digest/jobs/<date>-postings.json
   ```
   An exit 1, every board failed, is a failed fetch: stop and say so.
2. **Keep the ones that fit.** Read `jobs/profile/profile.txt` in full and
   `digest/jobs/<date>-postings.json`. Of the postings marked `new`, drop
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
3. **Rank** as `/job:apply`'s Shortlist states.
4. **Compose** `digest/jobs/<date>.json` (`digest_mail.py --help`):
   - **title** — the top posting as one sentence with a verb, naming the
     company and the work, such as `Tailscale is hiring a security
     engineer, remote from Canada`; with none kept, `No new niche
     posting fits today`.
   - **subtitle** — `<boards> niche boards read <date>: <new> new
     postings, <kept> fit`, and each board that failed, by name.
   - **items** — every kept posting, ranked: as headline `<Company> —
     <Role>`; as source `<area>; first posted <posted>; <location rule>;
     <pay or "pay not stated">`; as what, the work in two sentences in
     the posting's terms; as why, the profile facts it fits and each gap,
     quoting the profile's words; and its url as link.
   - **why_label** — `Why it fits`.
   - **skip** — each new posting dropped, `<Company> — <Role>: <why
     not>`, the reason in a few words.
   - **note** — that `/job:apply <url>` queues and applies to one, and
     that the boards are `.claude/skills/jobs:digest/boards.tsv`, which
     `/job:apply`'s search grows.
5. **Send, then mark.**
   ```bash
   .claude/scripts/digest_mail.py digest/jobs/<date>.json --stamp .claude/digest/jobs.last \
     && .claude/scripts/board_watch.py mark digest/jobs/<date>-postings.json --seen digest/jobs/seen.tsv
   ```
   A refusal is reported as it prints, and nothing is retried; the
   postings stay new for the next run.
