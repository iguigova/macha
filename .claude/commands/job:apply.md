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

3. **Build exclusion list.** Read all filenames in `jobs/done/` and grep URLs from those files.
   This gives you a set of (company, role, URL) tuples to skip. Normalize company/role names
   aggressively: lowercase, "Full Stack"="Fullstack"="Full-Stack", "Sr."="Senior", "Jr."="Junior".

4. **Source ladder.** Try sources in tier order. Stop as soon as you have ≥ N candidates that pass the filter gate and score ≥ 2 on the fit rubric.

   **Tier 1 — Structured sources (WebFetch, 1 fetch = many jobs):**

   | Source | URL | Notes |
   |--------|-----|-------|
   | HN Who is Hiring | `https://hnhiring.com/technologies/go`, then `/typescript`, `/csharp` | Best historical hit rate. Filter for "remote" in text. |
   | Jobicy API | `https://jobicy.com/api/v2/remote-jobs?count=50&tag=backend`, then `&tag=software` | JSON. Filter `jobGeo` for "Anywhere"/"Americas"/"Canada"/"Worldwide". |
   | RemoteOK API | `https://remoteok.com/api` | JSON array (skip first element = metadata). Low yield but cheap. |
   | Ashby career pages | `https://jobs.ashbyhq.com/auditboard`, `https://jobs.ashbyhq.com/cohere` | HTML, parseable. Only pages with 5+ relevant hits historically. |

   Try each source one at a time. After each fetch, apply the filter gate and fit rubric to the results. If you have ≥ N candidates, stop. Otherwise continue to the next source.

   **Tier 2 — WebSearch (only if Tier 1 < N candidates):**

   Rules: NEVER include the year. NEVER include "Canada". Use `site:` operators. Skip aggregator URLs (indeed, linkedin, glassdoor, ziprecruiter).

   Queries to try in order:
   - `site:jobs.ashbyhq.com "senior software engineer" remote`
   - `site:jobs.lever.co "backend engineer" remote`
   - `site:boards.greenhouse.io "software engineer" remote`
   - `"software engineer" remote "REST API" golang OR "C#" OR typescript -"security clearance"`

   **Tier 3 — Playwright (only if Tiers 1+2 < N, and Playwright connected):**

   - LinkedIn (remote SWE search with Canada geo filter)
   - WeWorkRemotely (programming jobs category)
   - Working Nomads (development jobs)

5. **Filter gate** (before fetching full description). Apply to every candidate from any source:

   SKIP: exclusion list match, on-site only, US-auth-required without sponsorship, Europe/APAC-timezone-locked, title is iOS/Android/Mobile/ML Engineer/Data Scientist/PHP/Ruby-only, security clearance required.

   KEEP: remote worldwide/Americas/Canada, location unspecified, title ambiguous. Bias toward keeping.

6. **Fit rubric** (after reading full description). Score each candidate that passed the filter gate:

   | Points | Criterion |
   |--------|-----------|
   | +1 | Language match: Go, C#/.NET, Java/Kotlin, or TypeScript required |
   | +1 | Domain match: REST APIs, auth/authz, relational DBs, or compliance |
   | +1 | Seniority match: Senior or Staff level |
   | +1 | Infra match: AWS, Docker, K8s, CI/CD, or 0→1 building |
   | +1 | Location+comp: Canada-remote-OK, salary ≥ $90K CAD |
   | −1 | Hard gap: primary requirement is zero-experience tech |

   4-5 = Strong → include. 3 = Good → include. 2 = Stretch → include only if short on candidates. 0-1 = Skip.

7. **Present results** to the user:
   ```
   Found N jobs:
   1. {Company} — {Role} [{score}/5 {label}]
      +lang(...) +domain(...) −gap(...)
   2. ...
   Proceeding to apply.
   ```

**Phase 2: Apply** (for each job)

8. Navigate to the job's page with `browser_navigate`.

9. `browser_snapshot` to understand the page. Find and click the Apply button.
   - LinkedIn: prefer "Easy Apply" (modal flow). If only external "Apply" exists, follow it.
   - Other platforms: click whatever apply/submit button is available.
   - If login is required, tell the user and skip this job.

10. If the application form has a cover letter field (textarea or file upload):
   Generate a cover letter from profile facts — pick the 3-4 career facts most relevant to THIS job's requirements. Follow the writing voice facts from the profile.
   If the form accepts a file upload for cover letter (not a textarea), generate a PDF:
   write the cover letter as HTML, then convert with `soffice --headless --convert-to pdf`.
   Save the PDF to `jobs/done/{company_role}_cover_letter.pdf`.
   If no cover letter field exists, skip this step.

11. Fill the form using profile facts:
    - Identity: name, email, phone, location, LinkedIn, GitHub, website
    - Upload resume: `~/Downloads/IlkaGuigova+.pdf` via `browser_file_upload`
    - Paste cover letter if a textarea is available, or upload PDF if file upload
    - Answer screening questions from profile facts:
      - Factual (yes/no, years, dropdown): look up the relevant fact
      - Behavioral (free-form): compose 3-5 sentences from career facts
      - Unknown: leave blank and flag for user
    - For demographics (gender, veteran, disability, race): use profile facts

12. **PAUSE** — ask the user with `AskUserQuestion`:
    > "Ready to submit to {Company} — {Role}:
    > - [summary of key answers filled]
    > - [any blanks or uncertainties]
    > Approve, correct, or skip?"
    Options: "Approve — submit", "Skip — next job", and user can type corrections via Other.

13. Handle response:
    - **Approve** → click submit → `browser_snapshot` to verify confirmation → save to `jobs/done/`
    - **Correct** → apply the corrections to the form → add each correction as a new fact in `jobs/profile/profile.txt` (if a fact contradicts an existing one, update the existing fact instead of adding a duplicate) → pause again
    - **Skip** → log as skipped → move to next job

14. On success, write `jobs/done/{company_role}.md` with:
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

15. On failure (form error, login wall, broken page), tell the user what happened and move to the next job.

16. After all jobs processed, report:
    > "Applied: X, Skipped: Y, Failed: Z
    > - {Company}: {Role} (applied|skipped|failed: reason)
    > - ...
    > New facts learned: N"

17. Append the report to `.claude/session_history.md` under `## {date} /job:apply`.
