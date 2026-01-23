# Macha Job Pipeline - Final Plan

## Architecture: Five Commands

### `/job:source`
Validate → Remove Inactive → Discover APIs → Discover Career Pages → Update Scrape Command

### `/job:scrape`
Fetch All Sources → Dedupe → Filter → Queue

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
│   ├── last_scrape                  # ISO 8601 timestamp of last scrape (date filter)
│   ├── queue/                       # Jobs to analyze
│   │   └── {company}_{role}.md
│   └── applications/                # Analyzed jobs
│       └── {company}_{role}.md
│
├── .claude/commands/
│   ├── job:source.md
│   ├── job:scrape.md
│   ├── job:analyze.md
│   ├── job:interview.md             # (future)
│   └── job:answer.md                # (future)
```

---

## sources.txt

```
# JSON APIs (no auth required)
https://remoteok.com/api
https://remotive.com/api/remote-jobs?category=software-development
https://jobicy.com/api/v2/remote-jobs?count=50&tag=developer
https://jobicy.com/api/v2/remote-jobs?count=50&tag=software
https://jobicy.com/api/v2/remote-jobs?count=50&tag=qa
https://jobicy.com/api/v2/remote-jobs?count=50&tag=frontend
https://jobicy.com/api/v2/remote-jobs?count=50&tag=backend
https://www.workingnomads.com/api/exposed_jobs/

# HTML sources
https://weworkremotely.com/categories/remote-programming-jobs

# Company career pages (added by /job:source)
https://www.shopify.com/careers/disciplines/engineering-data
https://jobs.ashbyhq.com/wrapbook
https://jobs.ashbyhq.com/auditboard
https://jobs.ashbyhq.com/fieldguide
https://jobs.ashbyhq.com/tenex
https://jobs.ashbyhq.com/cohere

# Playwright sources (require authenticated browser)
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

## Command 1: `/job:source`

`.claude/commands/job:source.md`

Validates existing sources, removes inactive ones, discovers new job board APIs and company career pages, and updates the scrape command's source handling.

**Steps:**
1. Read jobs/sources.txt
2. Validate each source (fetch, check for fresh relevant jobs)
3. Remove inactive sources (stale, empty, dead) from file entirely
4. Discover new job board APIs via web search
5. Discover company career pages hiring remote devs/QA
6. Update .claude/commands/job:scrape.md step 3 with any new source types
7. Report stats

---

## Command 2: `/job:scrape`

`.claude/commands/job:scrape.md`

```markdown
---
description: Scrape jobs from sources
allowed-tools: mcp__playwright__*, WebFetch, Read, Write, Grep, Bash
---

Scrape job listings and queue new ones for analysis.

**Steps:**

1. Read jobs/sources.txt (skip comment lines)

2. Read jobs/last_scrape (ISO 8601 timestamp). If missing, treat as first run.

3. Fetch from ALL sources (respect URL params, max 50 per URL otherwise)

4. For each source, fetch using appropriate method:
   - JSON APIs: WebFetch, parse response
   - HTML pages: WebFetch, parse listings
   - Playwright: authenticated browser (LinkedIn, Indeed)

5. **DATE FILTER** (JSON APIs only): Skip entries with publication date older than last_scrape timestamp. Uses date fields: epoch/date (RemoteOK), publication_date (Remotive), pubDate (Jobicy), pub_date (Working Nomads). HTML sources skip this step.

6. **DEDUPE**: Check company+role key against jobs/seen.txt - skip if exists

7. **FILTER**: Keep titles containing:
   - software, developer, backend, frontend, fullstack
   - QA, test, quality
   - Skip: manager, director, designer, data scientist, ML, machine learning, devops

8. For each passing job: fetch description, save to queue, append to seen.txt

9. Write current ISO 8601 timestamp (UTC) to jobs/last_scrape

10. Report: "Queued X jobs (fetched Y, old Z, deduped D, filtered F)"

11. Append report to .claude/session_history.md
```

---

## Command 3: `/job:analyze`

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
/job:source            # Validate sources, discover new ones, update scrape command
/job:scrape            # Fetch all sources, dedupe, filter, queue
/job:analyze all       # Process all queued jobs
```

---

## Files to Create

1. `.claude/commands/job:source.md`
2. `.claude/commands/job:scrape.md`
3. `.claude/commands/job:analyze.md`
4. `.claude/commands/job:interview.md` (future)
5. `.claude/commands/job:answer.md` (future)
6. `jobs/profile/profile.txt`
7. `jobs/profile/cover_letter_style.md`
8. `jobs/sources.txt`
9. `jobs/seen.txt` (empty)
10. `jobs/last_scrape` (created automatically on first scrape)
11. `jobs/queue/` directory
12. `jobs/applications/` directory

---

## Efficiency Notes

- **Source validation first**: Run `/job:source` to prune dead sources before scraping
- **Date filter (JSON APIs)**: `jobs/last_scrape` timestamp skips entries already processed in previous runs
- **Dedup before filter**: Don't waste time filtering already-seen jobs
- **Company+role key**: Catches cross-source duplicates (same job on multiple boards)
- **Single seen.txt**: Flat file, one company_role key per line, Grep tool (ripgrep) for dedup
- **Batch analyze**: `/job:analyze all` processes entire queue
- **Playwright session**: Persists login, no re-auth needed for ~30 days
- **Career pages**: Direct company pages often have roles not yet on aggregators
- **Session history**: All commands log reports to `.claude/session_history.md`

---

## Verification

```bash
# Test source validation
/job:source
# Check: dead sources removed, new sources added, scrape command updated

# Test scrape
/job:scrape
# Check: jobs/queue/ has files, jobs/seen.txt updated

# Test analyze
/job:analyze jobs/queue/[file].md
# Check: jobs/applications/ has result, queue file deleted

# Test dedup
/job:scrape
# Check: same company+role keys not added again
```
