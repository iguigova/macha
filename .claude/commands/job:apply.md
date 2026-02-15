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

3. Build an exclusion list, then search:

   a. **Build exclusion list first.** Read all filenames in `jobs/done/` and grep URLs from those files.
      This gives you a set of (company, role, URL) tuples to skip. Normalize company/role names
      aggressively: lowercase, "Full Stack"="Fullstack"="Full-Stack", "Sr."="Senior", "Jr."="Junior".
      This list is cheap to build (small files) and prevents wasted work.

   b. **Search with early filtering.** Use available sources to find candidates:
      - WebSearch for recent postings (vary queries based on profile strengths)
      - Job board APIs via WebFetch (RemoteOK: https://remoteok.com/api, Remotive: https://remotive.com/api/remote-jobs)
      - LinkedIn via Playwright if logged in
      - Direct career pages
      - Prefer jobs posted in the last 7 days.

      As each result comes in, immediately check it against the exclusion list.
      Skip matches silently — don't even fetch the job description.
      Also skip: requires security clearance, on-site only, wrong country without remote option.
      Bias toward applying — volume matters. If in doubt, keep it.

   c. **Pacing:**
      - N ≤ 3: Search one source at a time. Assess results before trying another.
      - N > 3: Search multiple sources in parallel.
      - Target is "at least N" — more is fine, fewer is acceptable if sources run dry.
        Report honestly what was found.

4. Present results to the user:
   > "Found N jobs matching your profile:
   > 1. {Company} — {Role}: {fit summary}
   > 2. {Company} — {Role}: {fit summary}
   > ...
   > Proceeding to apply."

**Phase 2: Apply** (for each job)

5. Navigate to the job's page with `browser_navigate`.

6. `browser_snapshot` to understand the page. Find and click the Apply button.
   - LinkedIn: prefer "Easy Apply" (modal flow). If only external "Apply" exists, follow it.
   - Other platforms: click whatever apply/submit button is available.
   - If login is required, tell the user and skip this job.

7. If the application form has a cover letter field (textarea or file upload):
   Generate a cover letter from profile facts — pick the 3-4 career facts most relevant to THIS job's requirements. Follow the writing voice facts from the profile.
   If the form accepts a file upload for cover letter (not a textarea), generate a PDF:
   write the cover letter as HTML, then convert with `soffice --headless --convert-to pdf`.
   Save the PDF to `jobs/done/{company_role}_cover_letter.pdf`.
   If no cover letter field exists, skip this step.

8. Fill the form using profile facts:
    - Identity: name, email, phone, location, LinkedIn, GitHub, website
    - Upload resume: `~/Downloads/IlkaGuigova+.pdf` via `browser_file_upload`
    - Paste cover letter if a textarea is available, or upload PDF if file upload
    - Answer screening questions from profile facts:
      - Factual (yes/no, years, dropdown): look up the relevant fact
      - Behavioral (free-form): compose 3-5 sentences from career facts
      - Unknown: leave blank and flag for user
    - For demographics (gender, veteran, disability, race): use profile facts

9. **PAUSE** — ask the user with `AskUserQuestion`:
    > "Ready to submit to {Company} — {Role}:
    > - [summary of key answers filled]
    > - [any blanks or uncertainties]
    > Approve, correct, or skip?"
    Options: "Approve — submit", "Skip — next job", and user can type corrections via Other.

10. Handle response:
    - **Approve** → click submit → `browser_snapshot` to verify confirmation → save to `jobs/done/`
    - **Correct** → apply the corrections to the form → add each correction as a new fact in `jobs/profile/profile.txt` (if a fact contradicts an existing one, update the existing fact instead of adding a duplicate) → pause again
    - **Skip** → log as skipped → move to next job

11. On success, write `jobs/done/{company_role}.md` with:
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

13. On failure (form error, login wall, broken page), tell the user what happened and move to the next job.

14. After all jobs processed, report:
    > "Applied: X, Skipped: Y, Failed: Z
    > - {Company}: {Role} (applied|skipped|failed: reason)
    > - ...
    > New facts learned: N"

15. Append the report to `.claude/session_history.md` under `## {date} /job:apply`.
