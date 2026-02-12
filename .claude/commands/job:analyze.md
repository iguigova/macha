---
description: Analyze fit and generate cover letter
allowed-tools: Read, Write, Glob
---

Analyze job fit and generate cover letter.

**Input:** $ARGUMENTS = queue file path, or "all" to process entire queue

**Steps:**

1. If $ARGUMENTS is "all":
   - Glob jobs/queue/*.md
   - Process each file
   Otherwise:
   - Process the single specified file

2. For each job file:

   a. Read the job from queue file

   b. Read jobs/profile/profile.txt

   c. **Select relevant facts**: Read the job requirements. Identify the 3-4 most relevant career facts from the profile. Use those specific facts (projects, metrics, tech) when composing the fit assessment and cover letter. Don't summarize the whole career — pick what matters for THIS job.

   d. **Fit Assessment**: Am I a good fit for this job?
      - Compare requirements to my skills
      - Note where I align (be specific - mention relevant projects)
      - Note gaps honestly
      - Rate: Strong fit / Good fit / Stretch / Poor fit

   e. **Generate Cover Letter** following the cover letter style facts in the profile:
      - Direct, factual, no fluff
      - No "excited" or "passionate"
      - No "20+ years"
      - Single page
      - Structure: Opening → Why fit → Technical alignment → What I bring → Close

   f. Save to jobs/applications/{company}_{role}.md:
      ```
      # {Company} - {Role}

      **URL:** {url}
      **Fit:** {Strong fit / Good fit / Stretch / Poor fit}

      ## Job Description
      {description}

      ## Fit Assessment
      {assessment}

      ## Cover Letter
      {letter}
      ```

   g. Delete the queue file (move it to applications/)

   h. Show the cover letter for review

3. Report:
   - "Analyzed X jobs, saved to jobs/applications/"
   - List each analyzed job: `- {company}: {role} ({fit rating})`

4. Log to session history: Append the report (from step 3) to `.claude/session_history.md` under a new dated heading `## {date} /job:analyze`.
