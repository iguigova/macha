---
description: Apply to one job from its link
argument-hint: <job-url>
allowed-tools: mcp__playwright__*, WebFetch, Read, Write, Edit, Bash(google-chrome:*), Bash(pdfinfo:*), Bash(pdftotext:*), Bash(git:*), AskUserQuestion
---

Apply to the job at $ARGUMENTS. Every claim in every document and answer
comes from a fact in `jobs/profile/profile.txt`; nothing is invented, and a
requirement no fact covers is a gap, not a sentence. The user approves twice:
the documents before the form, and the form before submit.

1. **Read the profile.** Read `jobs/profile/profile.txt` in full. If
   $ARGUMENTS is not a URL, ask for one and stop.

2. **Read the job.** `browser_navigate` to the URL, `browser_snapshot`, and
   read the whole description: title, location rule, salary range,
   requirements, preferred qualifications. If the page needs a login, tell
   the user and stop.

3. **Draft.** Write three things, each from profile facts only and each
   following the writing-voice facts:
   - **Gap analysis** — each requirement, paired with the fact that meets
     it or marked as a gap with how the user would close it. Flag a salary
     range or location rule that conflicts with the profile.
   - **Resume** — one page in the format the resume fact gives, with
     accomplishments ordered by relevance to this job.
   - **Cover letter** — one page, in the flow the cover-letter fact gives.

4. **PAUSE — review the drafts.** Show all three, then `AskUserQuestion`:
   "Approve, revise, or skip {Company} — {Role}?" with options "Approve —
   continue to form" and "Skip"; revisions come through Other. On a
   revision, redraft and pause again. On skip, stop. A revision that states
   a fact (not a wording change) also updates `profile.txt` — see step 8.

5. **Render the PDFs.** Write each approved document as HTML in `out/`
   (`@page { size: Letter; margin: 0.6in }`), then print it:
   ```bash
   google-chrome --headless --no-pdf-header-footer \
     --print-to-pdf=out/IlkaGuigova_Resume.pdf out/resume.html
   ```
   and the same for `out/IlkaGuigova_CoverLetter.pdf`. `pdfinfo` must
   report `Pages: 1` for each; if not, tighten the HTML and print again.
   `out/` holds only the current application and is overwritten each run.

6. **Fill the form.** Click Apply (on LinkedIn prefer Easy Apply). Then:
   - Identity: name, email, phone, location, LinkedIn, GitHub, website.
   - Upload `out/IlkaGuigova_Resume.pdf` with `browser_file_upload`.
   - Cover letter: paste the text into a textarea, or upload
     `out/IlkaGuigova_CoverLetter.pdf`. No field, no cover letter.
   - Screening questions: factual ones (yes/no, years, dropdowns) from the
     matching fact; free-form ones in 3–5 sentences from career facts.
   - Demographics: from the profile's EEO facts.
   - A question no fact answers: leave it blank and flag it.

7. **PAUSE — review the form.** `AskUserQuestion`: "Ready to submit to
   {Company} — {Role}" with a summary of every answer and every blank, and
   options "Approve — submit" and "Skip"; corrections come through Other.
   - **Approve** → submit, `browser_snapshot`, and confirm the page shows a
     confirmation. Report what it shows.
   - **Correct** → change the form, update the profile (step 8), pause
     again.
   - **Skip** → stop without submitting.

8. **Update the profile.** Each correction that states a fact updates the
   one line in `profile.txt` that covers it, or adds a line where none
   does; a fact never appears twice. When the run ends, commit
   `jobs/profile/profile.txt` alone with a message naming each fact changed.
