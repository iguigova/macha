---
description: Answer screening questions for job applications
allowed-tools: Read, Write, Edit, Glob, Grep, AskUserQuestion
---

Generate an answer to a screening question using the profile facts.

**Input:** $ARGUMENTS = the question text, optionally followed by `--app <application_file>`

**Steps:**

1. Read `jobs/profile/profile.txt`

2. Find or compose the answer:

   - **Factual questions** (yes/no, numeric, dropdown — e.g. "Are you authorized to work in Canada?", "Years of Python?"):
     Find the relevant fact(s) in the profile and return the value directly.

   - **Behavioral questions** (free-form — e.g. "Describe a challenging bug you debugged"):
     Read the career facts and compose a 3-5 sentence answer using specific projects and metrics. Follow the cover letter style facts (direct, concrete, no "excited"/"passionate"/"20+ years").

   - **Role-specific questions** ("Why do you want to work at {Company}?"):
     If `--app` is provided, read the job description and fit assessment from the application file. Compose an answer that connects profile facts to specific job requirements. Don't use generic praise — reference something concrete from the job description.

3. If the answer can't be found or inferred from the profile:
   - Use `AskUserQuestion` to ask the user
   - Add the new fact to `jobs/profile/profile.txt` in the appropriate group (Identity, Experience, Career, etc.)
   - If a new fact contradicts an existing one, update the existing fact in-place

4. If `--app <file>` is provided, append the Q&A to the application file under a `## Screening Answers` section (create if it doesn't exist):
   ```
   ## Screening Answers

   **Q: Describe a challenging bug you debugged**
   At Vivonet, I refactored a payment processing system...

   **Q: Years of experience with Go?**
   1 year professionally, with additional functional programming background.
   ```

5. Display the answer for review.

**Answer quality rules:**
- Draw from specific projects with metrics (same standard as cover letters)
- Never use "excited," "passionate," or "20+ years"
- Keep behavioral answers to 3-5 sentences
- For low experience values, add context (e.g. "1 year Go, plus functional programming background in Haskell, Erlang, and Clojure")
- For "why this company" questions, reference something specific from the job description

**Profile update rules:**
- New facts go in the appropriate existing group
- Changed facts replace the existing statement — never add a second version
- The profile must remain readable top to bottom after any update
