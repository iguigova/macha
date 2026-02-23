---
description: Find matching jobs and apply
allowed-tools: mcp__playwright__*, WebSearch, WebFetch, Read, Write, Edit, Glob, Grep, AskUserQuestion
---

Find jobs matching the profile, assess fit, and apply. Human-in-the-loop: pause before every submit, learn from corrections.

**Input:** $ARGUMENTS = a number (how many jobs to find and apply to), OR a URL (apply directly to this job)

**Phase 1: Find**

1. Read `jobs/profile/profile.txt`.

2. Determine what to do from $ARGUMENTS:
   - If it's a URL → skip to Phase 2 with that single job
   - If it's a number N → search for at least N matching jobs
   - If empty → treat as 1

3. **Build exclusion list.** Read `jobs/done/.exclusions` — each line is a SHA-256 hash of a previously-applied URL. If the file doesn't exist, rebuild it: read URLs from all `jobs/done/*.md` files, hash each with SHA-256, write one hash per line to `jobs/done/.exclusions`, then proceed.

4. **Search.** Use any combination of these sources. Start where you're most likely to find results quickly. Stop when you have ≥ N candidates.

   Sources (use judgment on order — no rigid tiers):
   - **LinkedIn** (via Playwright if connected) — search remote software engineer, filter by skills/location
   - **RemoteOK API** — `https://remoteok.com/api` (JSON, skip first element)
   - **Jobicy API** — `https://jobicy.com/api/v2/remote-jobs?count=50&tag=backend` (JSON, has jobGeo)
   - **WebSearch** — vary queries, use `site:` for Greenhouse/Lever/Ashby boards
   - **HN Who is Hiring** — `https://hnhiring.com/technologies/go` (and `/typescript`, `/csharp`)
   - **Direct career pages** — Greenhouse/Lever/Ashby APIs for specific companies

   Rules:
   - Check each result against the exclusion list by hashing its URL (SHA-256) and looking for a match. Skip matches silently.
   - Bias toward keeping. "Remote US" is fine — many hire Canadians. Location unspecified is fine.
   - Email-apply jobs are fine — send a cover letter email in Phase 2 if there's no web form.
   - SKIP: on-site only, US-auth-required, Europe/APAC-timezone-locked, title is iOS/Android/Mobile/ML Engineer/Data Scientist/PHP/Ruby-only, security clearance required.

5. **Present results.**
   ```
   Found N jobs:
   1. {Company} — {Role} ({location}, {key tech})
      Why: {1 sentence on fit}
   2. ...
   Proceeding to apply.
   ```
   No numeric scores. Just a sentence on why it fits.

**Phase 2: Apply** (for each job)

6. Navigate to the job's page with `browser_navigate`.

7. `browser_snapshot` to understand the page. Find and click the Apply button.
   - LinkedIn: prefer "Easy Apply" (modal flow). If only external "Apply" exists, follow it.
   - Other platforms: click whatever apply/submit button is available.
   - If login is required, tell the user and skip this job.

8. If the application form has a cover letter field (textarea or file upload):
   Generate a cover letter from profile facts — pick the 3-4 career facts most relevant to THIS job's requirements. Follow the writing voice facts from the profile.
   If the form accepts a file upload for cover letter (not a textarea), generate a PDF:
   write the cover letter as HTML, then convert with `soffice --headless --convert-to pdf`.
   Save the PDF to `jobs/done/{company_role}_cover_letter.pdf`.
   If no cover letter field exists, skip this step.

9. Fill the form using profile facts:
    - Identity: name, email, phone, location, LinkedIn, GitHub, website
    - Upload resume: `~/Downloads/IlkaGuigova+.pdf` via `browser_file_upload`
    - Paste cover letter if a textarea is available, or upload PDF if file upload
    - Answer screening questions from profile facts:
      - Factual (yes/no, years, dropdown): look up the relevant fact
      - Behavioral (free-form): compose 3-5 sentences from career facts
      - Unknown: leave blank and flag for user
    - For demographics (gender, veteran, disability, race): use profile facts

10. **PAUSE** — ask the user with `AskUserQuestion`:
    > "Ready to submit to {Company} — {Role}:
    > - [summary of key answers filled]
    > - [any blanks or uncertainties]
    > Approve, correct, or skip?"
    Options: "Approve — submit", "Skip — next job", and user can type corrections via Other.

11. Handle response:
    - **Approve** → click submit → `browser_snapshot` to verify confirmation → save to `jobs/done/`
    - **Correct** → apply the corrections to the form → add each correction as a new fact in `jobs/profile/profile.txt` (if a fact contradicts an existing one, update the existing fact instead of adding a duplicate) → pause again
    - **Skip** → log as skipped → move to next job

12. On success, write `jobs/done/{company_role}.md` with:
    ```
    # {Company} — {Role}
    URL: {url}
    Applied: {ISO 8601 timestamp}

    ## Cover Letter
    {the cover letter text, or "N/A — no cover letter field"}

    ## Form Data
    - {field}: {value} (source: {profile fact or "deduced from ..."})
    - ...
    (List every field filled: identity, resume, cover letter, screening answers, demographics)

    ## Screening Answers
    - {question}: {answer} (source: {profile fact or "composed from ..."})
    - ...

    ## Facts Used
    - {list of profile facts that informed answers}

    ## Facts Learned
    - {any new facts added during this application, or "None"}

    ## Corrections Applied
    - {any user corrections during this application, or "None"}
    ```
    Then append the SHA-256 hash of the job URL as a new line in `jobs/done/.exclusions`.

13. On failure (form error, login wall, broken page), tell the user what happened and move to the next job.

14. After all jobs processed, report:
    > "Applied: X, Skipped: Y, Failed: Z
    > - {Company}: {Role} (applied|skipped|failed: reason)
    > - ...
    > New facts learned: N"

15. Append the report to `.claude/session_history.md` under `## {date} /job:apply`.
