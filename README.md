# Macha - Job Search Automation

Automated job search pipeline powered by Claude Code slash commands. Scrapes job boards, deduplicates listings, assesses fit against a profile, and generates tailored cover letters.

## Setup

```bash
# Add Playwright MCP for authenticated scraping (LinkedIn, Indeed)
claude mcp add --transport stdio playwright -- npx -y @microsoft/playwright-mcp
```

On first use with LinkedIn/Indeed, manually log in when the browser opens. The session persists for ~30 days.

## Commands

| Command | Description |
|---------|-------------|
| `/job:source` | Validate sources, remove inactive, discover new APIs + career pages |
| `/job:scrape` | Scrape all sources, dedupe, filter, queue |
| `/job:analyze [file\|all]` | Select relevant facts, assess fit, generate cover letter |
| `/job:answer <question> [--app file]` | Generate answer from profile facts, update profile |
| `/job:apply [file\|all]` | Auto-apply via browser: fill forms, upload resume, submit |

## Workflow

```bash
/job:source           # Validate → prune dead → discover new APIs + career pages
/job:scrape           # Fetch all sources → dedupe → filter → queue
/job:analyze all      # Select relevant facts → assess fit → cover letter → application
/job:apply all        # Browser automation → fill forms → submit → done/
```

## Structure

```
jobs/
├── profile/
│   └── profile.txt              # Fact-based profile (identity, experience, career, style)
├── sources.txt                  # Job board URLs to scrape
├── seen.txt                     # Seen company+role keys (dedup)
├── last_scrape                  # Last scrape timestamp (date filter for JSON APIs)
├── queue/                       # Jobs awaiting analysis
│   └── {company}_{role}.md
├── applications/                # Analyzed jobs with cover letters
│   └── {company}_{role}.md
└── done/                        # Successfully submitted applications
    └── {company}_{role}.md
```

## Sources

**JSON APIs (no auth):**
- [Remote OK](https://remoteok.com/api) - all remote jobs, bulk JSON
- [Remotive](https://remotive.com/api/remote-jobs) - category-filtered (software-development)
- [Jobicy](https://jobicy.com/api/v2/remote-jobs) - tag-filtered (developer, software, qa, frontend, backend)
- [Working Nomads](https://www.workingnomads.com/api/exposed_jobs/) - all remote, category-tagged

**HTML (WebFetch):**
- [We Work Remotely](https://weworkremotely.com) - programming categories
- [Shopify Careers](https://www.shopify.com/careers/disciplines/engineering-data) - remote Americas engineering
- [Wrapbook Careers](https://jobs.ashbyhq.com/wrapbook) - remote Canada engineering
- [AuditBoard Careers](https://jobs.ashbyhq.com/auditboard) - remote Canada engineering
- [Fieldguide Careers](https://jobs.ashbyhq.com/fieldguide) - remote USA, cybersecurity/audit
- [TENEX.AI Careers](https://jobs.ashbyhq.com/tenex) - remote USA, AI security
- [Cohere Careers](https://jobs.ashbyhq.com/cohere) - remote Canada, AI/ML

**Authenticated (Playwright):**
- LinkedIn Jobs
- Indeed

## How It Works

**Source Management** (`/job:source`):
1. Fetches each source URL and checks: responds, has jobs, fresh (< 7 days), relevant
2. Removes all inactive sources (stale, empty, dead) from sources.txt
3. Discovers new job board APIs via web search
4. Discovers company career pages hiring remote devs/QA
5. Updates the scrape command's source handling for new source types

**Scraping** (`/job:scrape`):
1. Reads `jobs/sources.txt` for board URLs (skips comment lines)
2. Reads `jobs/last_scrape` timestamp (skips entries older than this for JSON APIs)
3. Fetches from ALL sources, extracting descriptions from JSON API responses
4. Date-filters JSON API results - skips entries published before last scrape
5. Deduplicates: first within-batch (cross-source overlaps), then against `jobs/seen.txt`
6. Filters by normalized title (removes hyphens, case-insensitive match: software, developer, backend, frontend, fullstack, QA, test, quality, sdet)
7. Gets descriptions: JSON API jobs already have them; HTML source jobs are batch-fetched in parallel
8. Queues all passing jobs, writes timestamp to `jobs/last_scrape`
9. Reports full stats and logs to `.claude/session_history.md`

**Analysis** (`/job:analyze`):
1. Reads job description from queue
2. Reads fact-based profile from `jobs/profile/profile.txt`
3. Selects 3-4 most relevant career facts for this specific job
4. Rates fit: Strong / Good / Stretch / Poor
5. Generates targeted cover letter from selected facts
6. Saves complete application to `jobs/applications/`

**Answering** (`/job:answer`):
1. Reads the question and the profile facts
2. Factual questions: returns the value from the profile
3. Behavioral questions: composes 3-5 sentence answer from career facts
4. Unknown answers: asks user, adds new fact to profile
5. Optionally saves Q&A to application file

**Applying** (`/job:apply`):
1. Reads application file (URL, cover letter, fit rating)
2. Navigates to job URL via Playwright browser automation
3. Detects platform (LinkedIn, Indeed, Ashby, Greenhouse, Lever, generic)
4. Fills form fields from profile identity facts
5. Uploads resume (`IlkaGuigova+.pdf`), pastes cover letter
6. Handles screening questions using `/job:answer` logic
7. Takes screenshot before submit, then submits
8. Moves successful applications to `jobs/done/`
