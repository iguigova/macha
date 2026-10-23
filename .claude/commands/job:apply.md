---
description: Apply to one job from its link
argument-hint: <job-url>
allowed-tools: mcp__playwright__*, WebFetch, Read, Write, Edit, Bash(google-chrome:*), Bash(pdfinfo:*), Bash(pdftotext:*), Bash(pdftoppm:*), Bash(git:*), AskUserQuestion
---

Apply to the job at $ARGUMENTS. Macha lives at `~/src/oceanhue/macha`, where
Claude Code is started; every path below is relative to it. Every claim in every document and answer comes from a fact in
`jobs/profile/profile.txt`; nothing is invented, and a requirement no fact
covers is a gap, not a sentence. The user approves twice: the documents
before the form, and the form before submit.

1. **Read the profile.** Read `jobs/profile/profile.txt` in full. If
   $ARGUMENTS is not a URL, ask for one and stop.

2. **Read the job.** `browser_navigate` to the URL and read the whole
   description with `browser_evaluate` returning `document.body.innerText`:
   title, location rule, salary range, requirements, preferred
   qualifications. If the page needs a login, tell the user and stop.

3. **Draft.** Write three things, each from profile facts only and each
   following the writing-voice facts:
   - **Gap analysis** — each requirement, paired with the fact that meets
     it or marked as a gap with how the user would close it. Flag a salary
     range or location rule that conflicts with the profile.
   - **Resume** — one page in the format the resume fact gives.
   - **Cover letter** — one page, in the flow the cover-letter fact gives.

   Then read both as the hiring manager for this role would, and revise
   before showing them: the closest match to the role leads, every
   sentence reads as something a person would say aloud, and a fact the
   role weighs that the profile lacks is named to the user as a question,
   never written in.

4. **PAUSE — review the drafts.** Show all three, then `AskUserQuestion`:
   "Approve, revise, or skip {Company} — {Role}?" with options "Approve —
   continue to form" and "Skip"; revisions come through Other. On a
   revision, redraft and pause again. On skip, stop. A revision that states
   a fact or a writing preference also updates `profile.txt` — see step 8.

5. **Render the PDFs.** Write `out/resume.html` and `out/cover_letter.html`,
   each linking `../jobs/templates/style.css`, which holds the look; the
   HTML holds only content in this markup:
   - Both: `<header>` with `<h1>` name and `.contact`; `<main>`; `<footer>`
     with `.sites` (the three Urgh! sites) and an empty `.band`.
   - Resume: under the name a `.tagline`; one `<section>` per resume
     section, each an `<h2>` label and one content block; Skills as a
     `<dl>`; each employer an `.entry` with a `.row` of `.org` and `.when`
     and a `<ul>`. A second line of detail under an entry is a `<br>` and
     a `.role` span.
   - Cover letter: `<body class="letter">`, a `.meta` block with the date
     and "{Company} — {Role}", the paragraphs, and `.sign`.

   Print each:
   ```bash
   cd ~/src/oceanhue/macha/out
   google-chrome --headless --no-pdf-header-footer \
     --print-to-pdf=$PWD/IlkaGuigova_Resume.pdf file://$PWD/resume.html
   ```
   and the same for `IlkaGuigova_CoverLetter.pdf`. `pdfinfo` must report
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
   like any other value. A text field's `maxLength` is read before pasting into it, and
   text that does not fit is shortened, never cut off. Then:
   - Identity: name, email, phone, location, LinkedIn, GitHub, website.
   - Upload `out/IlkaGuigova_Resume.pdf`.
   - Cover letter: paste the text into a textarea, or upload
     `out/IlkaGuigova_CoverLetter.pdf`. No field, no cover letter.
   - Screening questions: factual ones (yes/no, years, dropdowns) from the
     matching fact; free-form ones in 3–5 sentences from career facts.
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
   - **Skip** → stop without submitting.

8. **Update the profile.** Each correction that states a fact or a writing
   preference updates the one line in `profile.txt` that covers it, or adds
   a line where none does; a fact never appears twice. When the run ends,
   commit `jobs/profile/profile.txt` alone with a message naming each fact
   changed:
   ```bash
   git -C ~/src/oceanhue/macha commit -m "<message>" -- jobs/profile/profile.txt
   ```
   Then say it is a safe point to `/compact`: the committed profile and
   the journal carry what the next application needs.
