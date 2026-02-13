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
   - If it's a number N → search for N matching jobs
   - If empty → treat as 1

3. Search for jobs using any available method:
   - WebSearch for recent postings (try queries like "remote software engineer", "remote backend developer", "remote C# developer", "remote fullstack developer" — vary based on profile strengths)
   - LinkedIn via Playwright if the browser is logged in
   - Job board APIs via WebFetch (RemoteOK: https://remoteok.com/api, Remotive: https://remotive.com/api/remote-jobs)
   - Direct career pages
   - Use whatever finds relevant, recent postings. Prefer jobs posted in the last 7 days.

4. Check each candidate against `jobs/done/` filenames to avoid re-applying (filename format: `{company}_{role}.md` with spaces replaced by underscores, lowercase).

5. For each candidate, read the job description and do a quick fit assessment — 2-3 sentences: what matches, what gaps exist.

6. Filter out obvious mismatches:
   - Requires security clearance
   - On-site only (not remote)
   - Wrong country without remote option
   - Completely unrelated with no transferable skills
   - Bias toward applying — volume matters. If in doubt, keep it.

7. Present results to the user:
   > "Found N jobs matching your profile:
   > 1. {Company} — {Role}: {fit summary}
   > 2. {Company} — {Role}: {fit summary}
   > ...
   > Proceeding to apply."

**Phase 2: Apply** (for each job that passes fit)

8. Navigate to the job's page with `browser_navigate`.

9. `browser_snapshot` to understand the page. Find and click the Apply button.
   - LinkedIn: prefer "Easy Apply" (modal flow). If only external "Apply" exists, follow it.
   - Other platforms: click whatever apply/submit button is available.
   - If login is required, tell the user and skip this job.

10. Generate a cover letter from profile facts — pick the 3-4 career facts most relevant to THIS job's requirements. Follow the cover letter style facts from the profile.

11. Fill the form using profile facts:
    - Identity: name, email, phone, location, LinkedIn, GitHub, website
    - Upload resume: `~/Downloads/IlkaGuigova+.pdf` via `browser_file_upload`
    - Paste cover letter if a textarea is available
    - Answer screening questions from profile facts:
      - Factual (yes/no, years, dropdown): look up the relevant fact
      - Behavioral (free-form): compose 3-5 sentences from career facts
      - Unknown: leave blank and flag for user
    - For demographics (gender, veteran, disability, race): use profile facts

12. Take `browser_take_screenshot` of the completed form.

13. **PAUSE** — ask the user with `AskUserQuestion`:
    > "Ready to submit to {Company} — {Role}:
    > - [summary of key answers filled]
    > - [any blanks or uncertainties]
    > - Screenshot saved
    > Approve, correct, or skip?"
    Options: "Approve — submit", "Skip — next job", and user can type corrections via Other.

14. Handle response:
    - **Approve** → click submit → `browser_snapshot` to verify confirmation → save to `jobs/done/`
    - **Correct** → apply the corrections to the form → add each correction as a new fact in `jobs/profile/profile.txt` (if a fact contradicts an existing one, update the existing fact instead of adding a duplicate) → re-screenshot → pause again
    - **Skip** → log as skipped → move to next job

15. On success, write `jobs/done/{company_role}.md` with:
    ```
    # {Company} — {Role}
    URL: {url}
    Applied: {ISO 8601 timestamp}

    ## Cover Letter
    {the cover letter used}

    ## Screening Answers
    - {question}: {answer}
    - ...

    ## Facts Learned
    - {any new facts added during this application}
    ```

16. On failure (form error, login wall, broken page), tell the user what happened and move to the next job.

17. After all jobs processed, report:
    > "Applied: X, Skipped: Y, Failed: Z
    > - {Company}: {Role} (applied|skipped|failed: reason)
    > - ...
    > New facts learned: N"

18. Append the report to `.claude/session_history.md` under `## {date} /job:apply`.
