# Macha Job Pipeline - Final Plan

## Architecture: Five Commands

### `/job:source`
Validate → Remove Inactive → Discover APIs → Discover Career Pages → Update Scrape Command

### `/job:scrape`
Fetch All Sources → Dedupe → Filter → Queue

### `/job:analyze`
Job + Profile → Fit Assessment → Cover Letter → Application

### `/job:answer`
Profile Facts + Question → Tailored Answer → Profile Enriched

### `/job:apply`
Application + Browser Automation → Fill Form → Screenshot → Submit → done/

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
│   │   └── profile.txt              # Fact-based profile (all data in one file)
│   ├── sources.txt                  # URLs of job boards
│   ├── seen.txt                     # Seen company+role keys (dedup)
│   ├── last_scrape                  # ISO 8601 timestamp of last scrape (date filter)
│   ├── queue/                       # Jobs to analyze
│   │   └── {company}_{role}.md
│   ├── applications/                # Analyzed jobs with cover letters
│   │   └── {company}_{role}.md
│   └── done/                        # Successfully submitted applications
│       └── {company}_{role}.md
│
├── .claude/commands/
│   ├── job:source.md
│   ├── job:scrape.md
│   ├── job:analyze.md
│   ├── job:answer.md
│   └── job:apply.md
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
   - JSON APIs: WebFetch, parse response (extract descriptions from API response)
   - HTML pages: WebFetch, parse listings
   - Playwright: authenticated browser (LinkedIn, Indeed)

5. **DATE FILTER** (JSON APIs only): Skip entries with publication date older than last_scrape timestamp. Uses date fields: epoch/date (RemoteOK), publication_date (Remotive), pubDate (Jobicy), pub_date (Working Nomads). HTML sources skip this step.

6. **DEDUPE** (two-phase): First deduplicate within-batch by key (cross-source overlaps). Then check against jobs/seen.txt.

7. **FILTER**: Normalize title (remove hyphens), then case-insensitive match:
   - Keep: software, developer, backend, frontend, fullstack, QA, test, quality, sdet
   - Skip: manager, director, designer, data scientist, ML, machine learning, devops

8. Get descriptions (JSON: from API response; HTML: batch-fetch job pages in parallel). Save to queue, append to seen.txt

9. Write current ISO 8601 timestamp (UTC) to jobs/last_scrape

10. Report: "Queued X jobs (fetched Y, old Z, deduped D, filtered F)"

11. Append report to .claude/session_history.md
```

---

## Command 3: `/job:analyze`

`.claude/commands/job:analyze.md`

Select relevant facts from profile, assess fit, generate targeted cover letter.

**Steps:**
1. Read the job from queue file (or "all" to process entire queue)
2. Read jobs/profile/profile.txt
3. Select 3-4 most relevant career facts for THIS job
4. Assess fit: Strong / Good / Stretch / Poor
5. Generate cover letter from selected facts (following cover letter style facts in profile)
6. Save to jobs/applications/{company}_{role}.md with fit rating
7. Delete the queue file
8. Show the cover letter

---

## Command 4: `/job:answer`

`.claude/commands/job:answer.md`

Generate answers to screening questions from profile facts.

**Steps:**
1. Read jobs/profile/profile.txt
2. Factual questions: return the value from the profile
3. Behavioral questions: compose 3-5 sentence answer from career facts
4. Unknown answers: ask user, add new fact to profile
5. Optionally append Q&A to application file (with `--app`)

---

## Command 5: `/job:apply`

`.claude/commands/job:apply.md`

Auto-apply via Playwright browser automation.

**Steps:**
1. Read profile and application file(s)
2. Navigate to URL, detect platform (LinkedIn, Indeed, Ashby, Greenhouse, Lever, Workday, generic)
3. Fill form from profile identity facts
4. Upload resume (`~/Downloads/IlkaGuigova+.pdf`)
5. Paste cover letter, handle screening questions (same logic as `/job:answer`)
6. Screenshot before submit, then submit
7. On success: move to jobs/done/. On failure: log reason, continue.

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
**Fit:** Strong fit

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
/job:analyze all       # Select relevant facts → assess fit → cover letter
/job:apply all         # Browser automation → fill forms → submit → done/
```

---

## Files

1. `.claude/commands/job:source.md`
2. `.claude/commands/job:scrape.md`
3. `.claude/commands/job:analyze.md`
4. `.claude/commands/job:answer.md`
5. `.claude/commands/job:apply.md`
6. `jobs/profile/profile.txt` — fact-based profile (single source of truth)
7. `jobs/sources.txt`
8. `jobs/seen.txt`
9. `jobs/last_scrape` (created automatically on first scrape)
10. `jobs/queue/` directory
11. `jobs/applications/` directory
12. `jobs/done/` directory

---

## Efficiency Notes

- **Source validation first**: Run `/job:source` to prune dead sources before scraping
- **Date filter (JSON APIs)**: `jobs/last_scrape` timestamp skips entries already processed in previous runs
- **Dedup before filter**: Don't waste time filtering already-seen jobs
- **Company+role key**: Catches cross-source duplicates (same job on multiple boards)
- **Single seen.txt**: Flat file, one company_role key per line, Grep tool (ripgrep) for dedup
- **Batch analyze**: `/job:analyze all` processes entire queue
- **Batch apply**: `/job:apply all` processes all applications
- **Fact-based profile**: Single source of truth — identity, career, answers all derived from same facts
- **Learning loop**: Unknown screening answers → ask user → add fact to profile → auto-answered next time
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
# Check: jobs/applications/ has result, queue file deleted, cover letter uses 3-4 specific facts

# Test answer
/job:answer "Are you authorized to work in Canada?"
# Check: returns "Yes" from profile facts

/job:answer "Describe a challenging bug you debugged" --app jobs/applications/[file].md
# Check: 3-5 sentence answer from career facts, appended to application file

# Test apply (dry run)
/job:apply jobs/applications/[file].md
# Check: navigates, fills form, screenshots, submits, moves to done/

# Test dedup
/job:scrape
# Check: same company+role keys not added again
```
