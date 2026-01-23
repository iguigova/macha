# Macha Job Pipeline - Final Plan

## Architecture: Four Commands

### `/job:scrape`
Playwright MCP → Dedupe → Filter → Fetch → Queue

### `/job:analyze`
Job + Profile → Fit Assessment → Cover Letter → Application

### `/job:interview` (future)
Application → Practice Interview Questions → Mock Answers

### `/job:answer` (future)
Profile + Application + Question → Tailored Answer

---

## Setup

```bash
# Add Playwright MCP (one time)
claude mcp add --transport stdio playwright -- npx -y @microsoft/playwright-mcp

# First use: manually log into LinkedIn when browser opens
```

---

## Files

```
macha/
├── jobs/
│   ├── profile/
│   │   ├── profile.txt              # User profile
│   │   └── cover_letter_style.md    # Cover letter guidelines
│   ├── sources.txt                  # URLs of job boards
│   ├── seen.txt                     # Seen company+role keys (dedup)
│   ├── queue/                       # Jobs to analyze
│   │   └── {company}_{role}.md
│   └── applications/                # Analyzed jobs
│       └── {company}_{role}.md
│
├── .claude/commands/
│   ├── job:scrape.md
│   ├── job:analyze.md
│   ├── job:interview.md             # (future)
│   └── job:answer.md                # (future)
```

---

## sources.txt

```
https://remoteok.com/api
https://weworkremotely.com
https://remotive.com/api/remote-jobs
https://linkedin.com/jobs
https://indeed.com/jobs
```

---

## seen.txt

```
acme_corp_senior_backend_engineer
marketerx_senior_devops_engineer
```

---

## Command 1: `/job:scrape`

`.claude/commands/job:scrape.md`

```markdown
---
description: Scrape jobs from sources
allowed-tools: mcp__playwright__*, WebFetch, Read, Write, Grep
---

Scrape job listings and queue new ones for analysis.

**Input:** $ARGUMENTS = number of jobs (default: 50)

**Steps:**

1. Read jobs/sources.txt

2. For each source, fetch job listings:
   - LinkedIn/Indeed: Playwright MCP
   - Others: WebFetch

3. **DEDUPE FIRST**: Check each company+role key against jobs/seen.txt
   - Skip if URL already exists

4. **THEN FILTER**: Keep only titles containing:
   - software, engineer, developer, backend, frontend, fullstack
   - QA, SDET, test, quality
   - Skip: manager, director, designer, data scientist

5. For each new job passing both checks:
   - Fetch full description
   - Save to jobs/queue/{company}_{role}.md:
     ```
     # {Company} - {Role}

     **URL:** {url}

     ## Description
     {job description}
     ```
   - Append company+role key to jobs/seen.txt

6. Report: "Added X jobs to queue"
```

---

## Command 2: `/job:analyze`

`.claude/commands/job:analyze.md`

```markdown
---
description: Analyze fit and generate cover letter
allowed-tools: Read, Write, Glob
---

Analyze job fit and generate cover letter.

**Input:** $ARGUMENTS = queue file path (or "all")

**Steps:**

1. Read the job from queue file

2. Read jobs/profile/profile.txt

3. **Fit Assessment**: Am I a good fit for this job?
   - Compare requirements to my skills
   - Note where I align
   - Note gaps honestly

4. Read jobs/profile/cover_letter_style.md

5. **Generate Cover Letter** following guidelines:
   - Direct, factual, no fluff
   - No "excited" or "passionate"
   - No "20+ years"
   - Single page
   - Opening → Why fit → Technical → What I bring → Close

6. Save to jobs/applications/{company}_{role}.md:
   ```
   # {Company} - {Role}

   **URL:** {url}

   ## Job Description
   {description}

   ## Fit Assessment
   {assessment}

   ## Cover Letter
   {letter}
   ```

7. Delete the queue file

8. Show the cover letter
```

---

## Queue File Format

```markdown
# Acme - Senior Backend Engineer

**URL:** https://linkedin.com/jobs/view/123

## Description
We are looking for a Senior Backend Engineer...
```

---

## Application File Format

```markdown
# Acme - Senior Backend Engineer

**URL:** https://linkedin.com/jobs/view/123

## Job Description
We are looking for a Senior Backend Engineer...

## Fit Assessment
Strong fit. The role requires REST API experience (I have ~15 years),
Go/Java (my primary languages), and OAuth knowledge (built Bread & Butter
with 35+ OAuth providers). Gap: They prefer Python experience.

## Cover Letter
Dear Hiring Team,

I'm applying for the Senior Backend Engineer position...
```

---

## Workflow

```bash
/job:scrape 100        # Scrape, dedupe, filter, queue
/job:analyze all       # Process all queued jobs
```

---

## Files to Create

1. `.claude/commands/job:scrape.md`
2. `.claude/commands/job:analyze.md`
3. `.claude/commands/job:interview.md` (future)
4. `.claude/commands/job:answer.md` (future)
5. `jobs/profile/profile.txt`
6. `jobs/profile/cover_letter_style.md`
7. `jobs/sources.txt`
8. `jobs/seen.txt` (empty)
9. `jobs/queue/` directory
10. `jobs/applications/` directory

---

## Efficiency Notes

- **Dedup before filter**: Don't waste time filtering already-seen jobs
- **URL only**: No source tracking needed, URL is unique identifier
- **Single seen.txt**: Flat file, one company_role key per line, grep for dedup
- **Batch analyze**: `/job:analyze all` processes entire queue
- **Playwright session**: Persists login, no re-auth needed for ~30 days

---

## Verification

```bash
# Test scrape
/job:scrape 5
# Check: jobs/queue/ has files, jobs/seen.txt updated

# Test analyze
/job:analyze jobs/queue/[file].md
# Check: jobs/applications/ has result, queue file deleted

# Test dedup
/job:scrape 5
# Check: same URLs not added again
```
