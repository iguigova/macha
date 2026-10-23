---
description: Find open jobs that fit the profile and queue them
argument-hint: "[what to look for]   (default: the profile's preferences)"
allowed-tools: WebSearch, WebFetch, mcp__playwright__*, Read, Bash(curl:*), Bash(jq:*), Bash(.claude/scripts/job_log.sh:*), Bash(~/.claude/scripts/journal.sh:*), Bash(~/.claude/scripts/journal_queue.sh:*), AskUserQuestion
---

Find open jobs that fit the profile and queue the ones the user approves.
$ARGUMENTS, when given, narrows or adds to what to look for. Macha lives at
`~/src/oceanhue/macha`, where Claude Code is started; every path below is
relative to it. A queued job is a note that `/job:apply` takes later (README.md,
The job log). This command queues; it never applies.

1. **Read the profile.** Read `jobs/profile/profile.txt` in full. The search
   follows its facts: where the user lives, where they may work without
   sponsorship, that they work only remotely, the salary expectation, the kind
   of work they look for, and the stack they prefer. The kind of work decides
   whether a posting fits; the preferred stack only ranks one posting above
   another and never excludes one. Nothing outside the profile and $ARGUMENTS
   narrows the search.

2. **Read the job log.**
   ```bash
   .claude/scripts/job_log.sh
   ```
   Each row is a job already seen, at its latest outcome. Step 4 checks every
   candidate against it.

3. **Search every source.** Collect candidates from each one, and note a
   source that failed rather than dropping it silently:
   - **Company boards.** List the whole board of every company a lead names
     and every company in the log, since one fitting posting often has
     siblings: `https://boards-api.greenhouse.io/v1/boards/<board>/jobs`,
     `https://api.ashbyhq.com/posting-api/job-board/<board>?includeCompensation=true`,
     `https://api.lever.co/v0/postings/<company>`. Keep the engineering roles
     whose location rule includes where the user may work. A board that
     answers 404 is under another ATS or another name; say which.
   - **HN Who's Hiring.** The newest thread is the first hit of
     `https://hn.algolia.com/api/v1/search_by_date?tags=story,author_whoishiring&query=hiring`
     whose title starts "Ask HN: Who is hiring?". Its comments come from
     `https://hn.algolia.com/api/v1/items/<objectID>`; keep the ones naming
     remote work open where the user may work.
   - **Remote job boards:**
     `https://remoteok.com/api`,
     `https://remotive.com/api/remote-jobs?category=software-development`,
     `https://jobicy.com/api/v2/remote-jobs?count=50&industry=engineering`,
     `https://www.workingnomads.com/api/exposed_jobs/`,
     `https://weworkremotely.com/categories/remote-programming-jobs.rss`, and
     the `https://canadajobs.works/` page.
   - **LinkedIn.** Its job search in the Playwright browser, remote only and
     from the past week:
     `https://www.linkedin.com/jobs/search/?keywords=<words>&location=<where the user may work>&f_WT=2&f_TPR=r604800`.
     Signed out, it still lists jobs; a login wall means the user logs in by
     hand, so say so and go on.
   - **Anything else** a `WebSearch` turns up that fits, named by where it
     came from. A `site:` filter in the query is ignored, so search by the
     kind of work instead.

4. **Check each candidate at its source.** A board listing is a lead; the
   posting is the fact. Follow each lead to the company's own posting, and
   confirm it is open from its ATS API, where a closed posting answers 404:
   - Greenhouse: `https://boards-api.greenhouse.io/v1/boards/<board>/jobs/<id>`
   - Lever: `https://api.lever.co/v0/postings/<company>/<id>`
   - Ashby: the job's id in `https://api.ashbyhq.com/posting-api/job-board/<board>`
   - Anything else: the posting page itself.
   Read the posting for its title, location rule, salary range and stack.
   Keep it only if it is open, can be done remotely from where the user may
   work, has a salary range that reaches the expectation or states none, and
   is the kind of work the profile looks for, in any stack. A posting whose URL the log
   holds is dropped. A posting at a company the log holds is kept and marked
   with that row, so the user decides.

5. **PAUSE — the shortlist.** Rank by fit, closest first: the kind of work
   first, then the preferred stack. Show one table:
   rank, company, role, location rule, salary, the profile facts it fits,
   the log row it shares a company with if any, and the URL. Under it, list
   each source and how many candidates it gave, and each source that failed.
   Then `AskUserQuestion`: "Queue which of these?" with options "All" and
   "None"; a choice of ranks comes through Other. On None, stop.

6. **Queue the approved jobs**, one journal call each, best fit first, so the
   best fit is the oldest note and `/job:apply` takes it first:
   ```bash
   ~/.claude/scripts/journal.sh --via /job:batch -t "job: <Company> — <Role>" \
     -b "queued: <url>" -b "<location rule>, <salary>; fits <the facts it fits>" \
     -n "<Company> — <Role>: <url>"
   ```
   Then show the queue:
   ```bash
   ~/.claude/scripts/journal_queue.sh --topic "job: " --show
   ```
   and say that `/job:apply` takes the oldest, one at a time.
