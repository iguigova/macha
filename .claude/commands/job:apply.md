---
description: Auto-apply to jobs using browser automation
allowed-tools: mcp__playwright__*, Read, Write, Edit, Glob, Grep, AskUserQuestion
---

Auto-apply to jobs using Playwright browser automation.

**Input:** $ARGUMENTS = file path, or "all" to process all of `jobs/applications/`

**Steps:**

1. Read `jobs/profile/profile.txt` (the single source of truth for all form data and screening answers).

2. If $ARGUMENTS is "all":
   - Glob `jobs/applications/*.md`
   - Process each file
   Otherwise:
   - Process the single specified file

3. For each application file:

   a. **Parse the file**: extract URL (from `**URL:**`), cover letter (from `## Cover Letter`), company, role

   b. **Navigate** to the URL with `browser_navigate`

   c. **Snapshot** the page with `browser_snapshot`

   d. **Detect platform** from URL and choose strategy:

      | URL contains | Platform | Strategy |
      |---|---|---|
      | `linkedin.com` | LinkedIn Easy Apply | Click "Easy Apply" button → multi-page modal |
      | `indeed.com` | Indeed | Click "Apply now" → form flow |
      | `jobs.ashbyhq.com` | Ashby | Click "Apply" → single form |
      | `boards.greenhouse.io` | Greenhouse | Click "Apply" → single form |
      | `jobs.lever.co` | Lever | Click "Apply" → single form |
      | `myworkdayjobs.com` | Workday | Click "Apply" → multi-step |
      | anything else | Generic | Snapshot → find apply button → fill |

   e. **Fill the form** using profile facts:
      - Name: Ilka Guigova
      - Email: iguigova@gmail.com
      - Phone: 604-338-4242
      - Location: Brackendale, BC, Canada
      - LinkedIn: https://www.linkedin.com/in/iguigova/
      - GitHub: https://github.com/iguigova
      - Website: https://urgh.weebly.com/

   f. **Upload resume**: `~/Downloads/IlkaGuigova+.pdf` via `browser_file_upload`

   g. **Paste cover letter** from the `## Cover Letter` section of the application file. If the form has a cover letter textarea, paste it there. If it only accepts file upload, skip.

   h. **Handle screening questions** — follow the same logic as `/job:answer`:
      - Factual questions (yes/no, years, dropdown): look up in profile facts
      - Behavioral questions (free-form): compose from career facts, 3-5 sentences
      - If the answer can't be found or inferred: use `AskUserQuestion` → add new fact to profile
      - Record all answers in the application file under `## Screening Answers`

   i. **Pre-submit screenshot**: Take `browser_take_screenshot` of the completed form before submitting.

   j. **Submit**: Click the submit/send/apply button.

   k. **Verify**: `browser_snapshot` after submit to check for confirmation message.

   l. **On success**:
      - Add `## Applied\n{ISO 8601 timestamp}` to the application file
      - Move the file to `jobs/done/`

   m. **On failure** (no apply button, login required, error after submit, external redirect):
      - Add `## Apply Failed\n{ISO 8601 timestamp}: {reason}` to the application file
      - Leave the file in `jobs/applications/`
      - Continue to the next file

4. Report:
   - "Applied: X, Failed: Y, New facts learned: Z"
   - List each: `- {company}: {role} (applied|failed: reason)`

5. Log to session history: Append the report to `.claude/session_history.md` under a new dated heading `## {date} /job:apply`.

**Platform-specific notes:**

**LinkedIn Easy Apply:**
- Click the "Easy Apply" button (NOT the "Apply" button which redirects to an external site)
- The modal has multiple pages — click "Next" to advance through each page
- Fill contact info, upload resume, answer questions on each page
- Final page has "Review" then "Submit application"
- If no "Easy Apply" button exists, the job uses external apply — log as "external application, skipped" and move on
- If login is required, tell the user to log in manually and retry

**Indeed:**
- Click "Apply now" button
- May ask to sign in — if so, tell the user to log in manually and retry
- Form typically has resume upload + screening questions
- Final "Submit your application" button

**Ashby / Greenhouse / Lever (standard ATS forms):**
- Usually a single-page form with: name, email, phone, resume upload, cover letter, LinkedIn URL, optional custom questions
- Use `browser_fill_form` for text fields, `browser_file_upload` for resume
- Cover letter may be a textarea or file upload — paste text if textarea, skip if file-upload only

**Generic / Unknown:**
- Take a snapshot and identify form fields by their labels
- Fill fields that match recognizable patterns (name, email, phone, etc.)
- If no application form is found, log "no application form detected" and skip
