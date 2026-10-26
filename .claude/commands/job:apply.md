---
description: Apply to one job, from its link or the queue; an empty queue is filled by a search first
argument-hint: "[<job-url> | <job-id>]   (default: the oldest queued job)"
allowed-tools: mcp__playwright__*, WebSearch, WebFetch, Bash(curl:*), Bash(jq:*), Read, Write, Edit, Bash(google-chrome:*), Bash(pdfinfo:*), Bash(pdftotext:*), Bash(pdftoppm:*), Bash(git:*), Bash(~/.claude/scripts/tooling_commit.sh:*), Bash(~/.claude/scripts/journal.sh:*), Bash(~/.claude/scripts/journal_queue.sh:*), Bash(.claude/scripts/job_log.sh:*), Bash(.claude/scripts/job_board.sh:*), AskUserQuestion
---

Apply to one job: $ARGUMENTS. Macha lives at `~/src/oceanhue/macha`, where
Claude Code is started; every path below is relative to it. Every claim in every document and answer comes from a fact in
`jobs/profile/profile.txt`, the GitHub site it names, or the credo the global CLAUDE.md loads; nothing is invented, and a requirement no fact
covers is a gap, not a sentence. The user approves twice: the documents
before the form, and the form before submit.

1. **Read the profile and pick the job.** Read `jobs/profile/profile.txt`
   in full, and the GitHub site it names. A queued job is a note, `<Company> — <Role>: <url>`, captured
   under a `job: <Company> — <Role>` topic; list them with
   ```bash
   ~/.claude/scripts/journal_queue.sh --topic "job: "
   ```
   The argument is a URL, or a queued job's id; with none, take the first
   row, the oldest. A URL that a queued job carries is that job. With no
   argument and no queued job, find jobs (below), queue the ones the user
   approves, and take the first. A URL that
   `.claude/scripts/job_log.sh` lists at any outcome but `queued` has been
   seen before: name its row to the user and ask before going on.

2. **Read the job.** `browser_navigate` to the URL and read the whole
   description with `browser_evaluate` returning `document.body.innerText`:
   title, location rule, salary range, requirements, preferred
   qualifications. If the page needs a login, tell the user and stop. If
   the posting is gone or no longer open, the job is closed: record it
   (step 9) and stop. A Greenhouse, Ashby or Lever posting is closed when
   `.claude/scripts/job_board.sh <ats> <board> <id>` exits 1.

3. **Draft.** Write three things, each from profile facts only and each
   following the credo voice and the application voice below:
   - **Gap analysis** — each requirement, paired with the fact that meets
     it or marked as a gap with how the user would close it. Flag a salary
     range or location rule that conflicts with the profile.
   - **Resume** — in the resume format below.
   - **Cover letter** — in the cover-letter format below.

   Then read both as the hiring manager for this role would, and revise
   before showing them: the closest match to the role leads.

   Then ask for what the profile lacks, before step 4 and never folded
   into its approval: one `AskUserQuestion` with one question per fact the
   role weighs that no fact covers, or that two facts state differently.
   Each question names the requirement and offers concrete answers, each
   option saying what it would write; a free answer comes through Other.
   Write each answer into the profile at once (step 8) and redraft from
   it. A fact is never written into a document before it is in the
   profile.

4. **PAUSE — review the drafts.** Show all three, then `AskUserQuestion`:
   "Approve, revise, or skip {Company} — {Role}?" with options "Approve —
   continue to form" and "Skip"; revisions come through Other. On a
   revision, redraft and pause again. On skip, record it (step 9) and stop. A revision that states
   a fact or a writing preference also updates the profile — see step 8.

5. **Render the PDFs.** Write `out/resume.html` and `out/cover_letter.html`,
   each linking `../jobs/templates/style.css`, which holds the look; the
   HTML holds only content in this markup:
   - Both: `<header>` with `<h1>` name and `.contact`; `<main>`; `<footer>`
     with `.sites` (the three themed websites the profile names) and an empty `.band`.
   - Resume: under the name a `.tagline`; one `<section>` per resume
     section, each an `<h2>` label and one content block; Skills as a
     `<dl>`; each employer an `.entry` with a `.row` of `.org` and `.when`
     and a `<ul>`. A second line of detail under an entry is a `<br>` and
     a `.role` span.
   - Cover letter: `<body class="letter">`, a `.meta` block with the date
     and "{Company} — {Role}", the paragraphs, and `.sign`.

   Print each, named for the user: `<Name>` is the profile's name with
   its spaces removed.
   ```bash
   cd ~/src/oceanhue/macha/out
   google-chrome --headless --no-pdf-header-footer \
     --print-to-pdf=$PWD/<Name>_Resume.pdf file://$PWD/resume.html
   ```
   and the same for `<Name>_CoverLetter.pdf`. `pdfinfo` must report
   `Pages: 1` for each; when it does not, `pdftotext -f 2` shows what
   spilled. Fit by balance: reword a line that leaves one word on the next,
   then adjust spacing in the stylesheet; a fact the job weighs is never
   dropped to fit. Check the look with `pdftoppm -png` before the form: no
   line ends with a lone word, and the space is even down to the footer. `out/` holds only the
   current application and is overwritten each run.

6. **Fill the form.** Click Apply (on LinkedIn prefer Easy Apply). A
   Greenhouse form embedded on the company's page sits in an iframe: fill
   it there through `page.frameLocator('iframe[src*="greenhouse"]')`,
   never by opening the iframe's URL, which blocks reCAPTCHA and makes
   Greenhouse demand an emailed code. A company that links to its
   `job-boards.greenhouse.io` page instead has the form on that page.
   Its dropdowns open with focus and `pressSequentially` (or `ArrowDown`),
   then a click on the option; `fill()` shows no options. After the resume
   upload its first file input is gone, so the cover letter goes to
   `#cover_letter`. On Rippling, a radio or the SMS opt-in is clicked
   through the label its `aria-labelledby` names, a file chip is removed by
   its last `div[role=button]`, and the resume upload pre-fills name,
   email, phone, company and location, which are read back and corrected
   like any other value. On Ashby, `fill()` can show a value the form
   never took and a label click can leave a radio unset; type into the
   field with `pressSequentially` and set a radio with
   `check({ force: true })` on its input. A refused submit lists each
   field it found empty. A text field's `maxLength` is read before pasting into it, and
   text that does not fit is shortened, never cut off. Then:
   - Identity: name, email, phone, location, LinkedIn, GitHub, website.
   - Upload `out/<Name>_Resume.pdf`.
   - Cover letter: paste the text into a textarea, or upload
     `out/<Name>_CoverLetter.pdf`. No field, no cover letter.
   - Screening questions: factual ones (yes/no, years, dropdowns) from the
     matching fact; free-form ones in the screening-answer format below,
     from career facts.
   - Demographics: from the profile's EEO facts.
   - A question no fact answers (how you heard of the job, a policy to
     agree to): leave it blank and ask the user, one `AskUserQuestion`
     with each question and its options.
   Read every value back from the form before the next step.

7. **PAUSE — review the form.** `AskUserQuestion`: "Ready to submit to
   {Company} — {Role}" with a summary of every answer and every blank, and
   options "Approve — submit" and "Skip"; corrections come through Other.
   - **Approve** → submit and read the page. If it asks for an emailed
     security code, ask the user for it at once: the code expires. A form
     left open for hours fails on submit, and a browser left idle
     overnight comes back on `about:blank`; reopen the page, refill the
     approved values, read them back and submit again. A letter drafted on
     an earlier day is redated and re-rendered before upload. Report the
     confirmation the page shows.
   - **Correct** → change the form, update the profile (step 8), pause
     again.
   - **Skip** → record it (step 9) and stop without submitting.

8. **Update the profile.** Each answer or correction that states a fact or
   a writing preference updates the one line that covers it, or adds a
   line where none does: a fact about the user in `profile.txt`, a rule
   for application writing under "Application voice" below, and any other
   writing preference in `~/.claude/credo.md`. A fact never appears twice, and one that
   contradicts another line is asked about, never written beside it. A
   number is quoted exactly as the user or the source states it, and a
   mixed result is stated whole. A fact drawn from work names no customer,
   account ID, hostname or colleague. Commit at once, after each round of
   answers or corrections, the files changed alone with a message naming
   each fact changed:
   ```bash
   git -C ~/src/oceanhue/macha commit -m "<message>" -- jobs/profile/profile.txt .claude/commands/job:apply.md
   ~/.claude/scripts/tooling_commit.sh -m "<message>" -- ~/.claude/credo.md
   ```

9. **Record the outcome.** Every run ends with one journal entry under the
   job's topic, its first bullet the outcome and the URL:
   ```bash
   ~/.claude/scripts/journal.sh --via /job:apply -t "job: <Company> — <Role>" \
     -b "<applied|closed|skipped>: <url>" -b "<what the page showed, or why>" \
     [--placed "<id>: - — <applied|closed|skipped>: <reason>"]
   ```
   `--placed` closes the queued job the run took; a URL nothing queued
   has no note to close.

10. **Check the profile's integrity**, by the principles in the credo.
    Read `profile.txt` whole and list, with line numbers
    and both lines quoted:
    - two lines that contradict: a span of years against an employer's
      dates, a claim against a "no experience" line, one fact stated two
      ways;
    - a fact stated twice;
    - a fact copied outside `profile.txt`, found by grepping its value in
      `CLAUDE.md`, `README.md` and `.claude/`.

    Ask about each contradiction, one question each: which line is wrong.
    The wrong line is deleted, or corrected only to what the user states;
    the check that found it never supplies the replacement. A duplicate is
    removed, and a copy becomes a pointer to its file. Commit as in
    step 8. With nothing found, say what was checked. Then say it is
    a safe point to `/compact`: the committed profile and the journal carry
    what the next application needs.

## Finding jobs

A search fills an empty queue. It follows the profile's facts: where the
user lives, where they may work without sponsorship, that they work only
remotely, the salary expectation, the kind of work they look for, and the
stack they prefer. The kind of work decides whether a posting fits; the
preferred stack only ranks one posting above another and never excludes
one. Nothing outside the profile narrows the search.

1. **Read the job log.**
   ```bash
   .claude/scripts/job_log.sh
   ```
   Each row is a job already seen, at its latest outcome. The check below
   holds every candidate against it.

2. **Search every source.** Collect candidates from each one, and note a
   source that failed rather than dropping it silently:
   - **Company boards.** List the whole board of every company a lead names
     and every company in the log, since one fitting posting often has
     siblings:
     ```bash
     .claude/scripts/job_board.sh <greenhouse|ashby|lever> <board>
     ```
     Keep the engineering roles whose location rule includes where the user
     may work. A board that answers "no board" is under another ATS or
     another name; say which.
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

3. **Check each candidate at its source.** A board listing is a lead; the
   posting is the fact. Follow each lead to the company's own posting, and
   confirm it is open: `.claude/scripts/job_board.sh <ats> <board> <id>`
   exits 0 while a Greenhouse, Ashby or Lever posting is open and 1 once it
   is closed. Anything else is checked on the posting page itself.
   Read the posting for its title, location rule, salary range and stack.
   Keep it only if it is open, can be done remotely from where the user may
   work, has a salary range that reaches the expectation or states none, and
   is the kind of work the profile looks for, in any stack. A posting whose
   URL the log holds is dropped. A posting at a company the log holds is
   kept and marked with that row, so the user decides.

4. **PAUSE — the shortlist.** Rank by fit, closest first: the kind of work
   first, then the preferred stack. Show one table: rank, company, role,
   location rule, salary, the profile facts it fits, the log row it shares a
   company with if any, and the URL. Under it, list each source and how many
   candidates it gave, and each source that failed. Then `AskUserQuestion`:
   "Queue which of these?" with options "All" and "None"; a choice of ranks
   comes through Other. On None, the run stops with no job.

5. **Queue the approved jobs**, one journal call each, best fit first, so the
   best fit is the oldest note and the first one taken:
   ```bash
   ~/.claude/scripts/journal.sh --via /job:apply -t "job: <Company> — <Role>" \
     -b "queued: <url>" -b "<location rule>, <salary>; fits <the facts it fits>" \
     -n "<Company> — <Role>: <url>"
   ```
   Then show the queue with `journal_queue.sh --topic "job: " --show` and
   read its first row as the job, from step 2 above.

## Application voice

These add to the credo voice, for letters and screening answers.

- Let the work speak. Examples, not labels: no jargon such as "B2B
  platforms" or "enterprise systems", no list of years and tools, no
  algorithms, publications or awards dropped to impress, and no count of
  languages or technologies ("various" reads more honest than "10+").
  Light framing (impact, scope, ownership) is fine.
- Keep a number only when it shows scale or impact (millions of
  transactions, 35+ identity providers, 40% faster). Leave out numbers
  that undersell: partial results, commit counts, small before-and-after
  figures.
- Their pitch is theirs. Never repeat a company's mission, stage or
  opportunity back to it, and never close on "I want to build that". No
  mission talk, no corporate enthusiasm, no meta commentary, and never
  "excited", "passionate" or "20+ years".
- Get into the work at once: no sweeping career opener, no positioning
  statement, no section headers such as "What I bring:", and no closing
  line that ties everything together.
- A personal question gets a genuine, short answer, not tied back to the
  job and with no "what it taught me".

## Application formats

- **Cover letter** — one page at most. Flow: the role and why this
  company, then concrete project examples, then the gaps stated honestly
  with how the user would close them, then "Thank you for your time and
  consideration."
- **Screening answer** — 3–5 sentences stating what the user built.
- **Resume** — one page at most, made for each job. Header: name, the
  tagline from the credo, the address `profile.txt` gives the resume,
  phone, email, CV link. Sections:
  Skills; Experience, by employer and dates, without job titles, newest
  first, the bullets most relevant to the job first; Community;
  Philosophy, from the credo; Education.
- Both documents share the look of the user's website urgh.weebly.com,
  kept in `jobs/templates/style.css` (step 5).
