---
name: job:apply
description: Apply to one job, from its link or the queue; an empty queue is filled by a search first
argument-hint: "[<job-url> | <job-id>]   (default: the oldest queued job)"
allowed-tools: mcp__playwright__*, WebSearch, WebFetch, Bash(curl *), Bash(jq *), Read, Write, Edit, Bash(google-chrome *), Bash(pdfinfo *), Bash(pdftotext *), Bash(pdftoppm *), Bash(test *), Bash(exit *), Bash(~/.claude/scripts/tooling_commit.sh *), Bash(~/.claude/scripts/journal.sh *), Bash(~/.claude/scripts/journal_queue.sh *), Bash(.claude/scripts/job_board.sh *), Bash(.claude/scripts/profile_lint.sh *), AskUserQuestion
---

Apply to one job: $ARGUMENTS. Read `.claude/skills/job:apply/steps.md`
first: its sections are the steps below that name one. The user approves
twice: the documents before the form, and the form before submit.
Every run ends with `browser_close`, whichever step it stops at, so the
browser is freed for the next session.

1. **Pick the job**, as Pick the posting.

2. **Read the job.** `browser_navigate` to the URL and read the whole
   description with `browser_evaluate` returning `document.body.innerText`:
   title, location rule, employment type, pay, requirements, preferred
   qualifications. If the page needs a login, tell the user and stop. If
   the posting is gone or no longer open (Check at source), the job is
   closed: close its note (step 9) and stop.

3. **Draft.** Write three things, each from profile facts only and each
   following the credo voice and the application voice below:
   - **Gap analysis** — each requirement, paired with the fact that meets
     it or marked as a gap with how the user would close it. Flag pay or a
     location rule that conflicts with the profile.
   - **Resume** — in the resume format below.
   - **Cover letter** — in the cover-letter format below.

   Then read both as the hiring manager for this role would, and revise
   before showing them: the closest match to the role leads. Then check
   the letter one sentence at a time against the credo voice and the
   application voice, the credo's out-loud test first; a sentence that fails is
   rewritten or cut, and the drafts are shown with each sentence's verdict.
   Then Ask for what the profile lacks.

4. **PAUSE — review the drafts.** Show all three, then `AskUserQuestion`:
   "Approve, revise, or skip {Company} — {Role}?" with options "Approve —
   continue to form" and "Skip"; revisions come through Other. On a
   revision, redraft and pause again. On skip, close its note (step 9) and
   stop. A revision that states a fact or a writing preference also
   updates the profile (step 8).

5. **Render the PDFs**, as Render the PDFs: `out/resume.html` and
   `out/cover_letter.html`, printed as `<Name>_Resume.pdf` and
   `<Name>_CoverLetter.pdf`, one page each.

6. **Fill the form.** Click Apply (on LinkedIn prefer Easy Apply). A
   Greenhouse form embedded on the company's page sits in an iframe: fill
   it there through `page.frameLocator('iframe[src*="greenhouse"]')`,
   never by opening the iframe's URL, which blocks reCAPTCHA and makes
   Greenhouse demand an emailed code. A company that links to its
   `job-boards.greenhouse.io` page instead has the form on that page.
   Its dropdowns open with focus and `pressSequentially` (or `ArrowDown`),
   then a click on an option matched with `[role=option]:visible`, since
   the phone field's hidden country list also has options; `fill()` shows
   no options. A demographic answer adds a required consent box for that
   data, which goes to the user with any other policy. After the resume
   upload its first file input is gone, so the cover letter goes to
   `#cover_letter`. On Rippling, a radio or the SMS opt-in is clicked
   through the label its `aria-labelledby` names, a file chip is removed by
   its last `div[role=button]`, and the resume upload pre-fills name,
   email, phone, company and location, which are read back and corrected
   like any other value. On Ashby, `fill()` can show a value the form
   never took and a label click can leave a radio unset; type into the
   field with `pressSequentially` and set a radio with
   `check({ force: true })` on its input. On Kula, a dropdown opens like
   Greenhouse's from its hidden input, with focus and `ArrowDown`; a text
   field is typed with `pressSequentially` after `focus()`, since a click
   can time out; the file inputs are `#resume` and `#cover_letter`; and a
   file replaced by one of the same name is cleared with
   `setInputFiles([])` before it is set again. On PageUp, a dropdown is
   a click on its `-edit` input, then on the visible `li`; an institution
   or company is chosen in a search popup, where a name its list lacks is
   "Other"; a file uploads as soon as its input is set; and the session
   times out within hours, keeping each page saved with Continue, and is
   resumed through the emailed one-time code, whose Login button is
   matched by role, then the application's `buildForm.asp` link. A
   refused submit lists each
   field it found empty. A text field's `maxLength` is read before pasting
   into it, and text that does not fit is shortened, never cut off. Each
   field's help text is read before it is filled, and a field that asks
   bots not to answer goes to the user, who decides. Then:
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
   - **Skip** → close its note (step 9) and stop without submitting.

8. **Update the profile**, as Update the profile.

9. **Close the note**, as Close the note.

10. **Check the profile's integrity**, as Check the profile's integrity.

## Finding jobs

A search fills an empty queue. Only the profile's facts narrow it, as Check at source and Shortlist apply them.

1. **Read the postings already seen**, open and closed; each note carries
   its URL:
   ```bash
   ~/.claude/scripts/journal_queue.sh --topic "job: "
   ~/.claude/scripts/journal_queue.sh --closed --topic "job: "
   ```
   Every candidate is held against them.

2. **Search every source** under Sources, and note a source that failed
   rather than dropping it silently.

3. **Check each candidate at its source.** A listing is a lead; the
   posting is the fact. Follow each lead to the posting itself and keep it
   only if Check at source finds it open and fitting. A posting whose URL
   a note carries is dropped. A posting from a company a note names is
   kept and marked with that note, so the user decides.

4. **PAUSE — the shortlist.** Rank as Shortlist states. Show one table:
   rank, the posting's name, the columns Shortlist names, the profile
   facts it fits, the note it shares a company with if any, and the URL.
   Under it, list each source and how many candidates it gave, and each
   source that failed. Then `AskUserQuestion`: "Queue which of these?"
   with options "All" and "None"; a choice of ranks comes through Other.
   On None, the run stops with nothing queued.

5. **Queue the approved postings**, one journal call each, best fit first,
   so the best fit is the oldest note and the first one taken:
   ```bash
   ~/.claude/scripts/journal.sh --via /job:apply -t "job: <Company> — <Role>" \
     -b "<url> — <location rule>, <pay>; fits <the facts it fits>" \
     -n "<Company> — <Role>: <url>"
   ```
   Then show the queue with `journal_queue.sh --topic "job: " --show`
   and take its first row.

**Sources:** one that answers JSON is read with
`curl -sS '<url>' | jq '<filter>'`.
- **Company boards.** List the whole board of every company a lead names,
  since one fitting posting often has siblings:
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
  Signed out, it still lists jobs. Each session's browser is isolated and
  starts from the saved logins; a login wall means the user runs
  `! ~/.claude/scripts/playwright_login.sh https://www.linkedin.com/login`,
  logs in and closes that window, which saves the login for every later
  session. Then `browser_close`, so the next call opens a browser that has
  the login. Signed in, it redirects to
  `/jobs/search-results/`, which can drop the location and remote filters,
  so each card's location is read. Its "Posted" age counts a repost; the
  posting's board gives its first date.
- **Anything else** a `WebSearch` turns up that fits, named by where it
  came from. A `site:` filter in the query is ignored, so search by the
  kind of work instead.

**Check at source.** `.claude/scripts/job_board.sh <ats> <board> <id>`
exits 0 while a Greenhouse, Ashby or Lever posting is open and 1 once it
is closed. Exit 4, the board itself is not there, and exit 3, its API
did not answer or answered otherwise, say nothing about the posting: on
4 find the company's board under another ATS or name, and on 3 read the
posting page. A posting on any other site is checked on its page.
Read the posting as step 2 does. It fits if it can be done remotely from where the user may work, its pay reaches the
profile's (a salary range the salary expectation, a contract or freelance
rate the freelance rate) or it states none, and it is the kind of work
the profile looks for, in any stack.

**Shortlist.** Postings the profile's niche line describes rank first;
a small company is one whose board lists about 30 open postings or
fewer. A posting from a larger board ranks below them unless its
problem is itself rare. Then rank by the kind of work, then the
preferred stack. A board's size is the row count of
`job_board.sh <ats> <board>`, or the postings its careers page lists.
The columns are company, role, board size, location rule and pay.

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
- An employer is described in the words its profile lines use, never by
  an industry they leave out.
- A personal question gets a genuine, short answer, not tied back to the
  job and with no "what it taught me".
- A question asking why this company gets a personal answer: one thing
  about the user, from the user's own pages such as the interview page,
  that connects to the role and repeats neither the letter nor the resume.
- The subject comes first and the verb early, as people speak: "I want to
  write more Go", never "Go is the stack I want to work in".
- A letter shows who the user is through one of the profile's lines
  about the user — what suits them, what they read, how they describe
  themselves — where it truly connects to the role. It is said from the
  user's side and names
  the role; it never describes the company's project back to it. No
  feeling is invented.

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
- Both documents share the base style, `~/src/oceanhue/brand.css`:
  `jobs/templates/style.css` imports it and adds the print layout (step 5).
