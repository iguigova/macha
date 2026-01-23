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
| `/job:analyze [file\|all]` | Assess fit and generate cover letter |
| `/job:interview` | Practice interview questions (planned) |
| `/job:answer` | Generate tailored answers from profile (planned) |

## Workflow

```bash
/job:source           # Validate → prune dead → discover new APIs + career pages
/job:scrape           # Fetch all sources → dedupe → filter → queue
/job:analyze all      # Assess fit → cover letter → save application
```

## Structure

```
jobs/
├── profile/
│   ├── profile.txt              # Skills, experience, projects
│   └── cover_letter_style.md    # Cover letter guidelines
├── sources.txt                  # Job board URLs to scrape
├── seen.txt                     # Seen company+role keys (dedup)
├── last_scrape                  # Last scrape timestamp (date filter for JSON APIs)
├── queue/                       # Jobs awaiting analysis
│   └── {company}_{role}.md
└── applications/                # Analyzed jobs with cover letters
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
3. Fetches from ALL sources (respects URL params like count=50, limit=20)
4. Date-filters JSON API results (RemoteOK, Remotive, Jobicy, Working Nomads) - skips entries published before last scrape
5. Deduplicates by company+role key against `jobs/seen.txt`
6. Filters by role title (software, developer, backend, frontend, fullstack, QA, test, quality)
7. Queues all passing jobs (never discards matches)
8. Writes current timestamp to `jobs/last_scrape`
9. Reports full stats and logs to `.claude/session_history.md`

**Analysis** (`/job:analyze`):
1. Reads job description from queue
2. Compares requirements against `jobs/profile/profile.txt`
3. Rates fit: Strong / Good / Stretch / Poor
4. Generates cover letter per `jobs/profile/cover_letter_style.md`
5. Saves complete application to `jobs/applications/`
