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
| `/job:scrape [n]` | Scrape job boards, dedupe, filter, queue (default: 50 jobs) |
| `/job:analyze [file\|all]` | Assess fit and generate cover letter |
| `/job:interview` | Practice interview questions (planned) |
| `/job:answer` | Generate tailored answers from profile (planned) |

## Workflow

```bash
/job:scrape 100       # Scrape → dedupe → filter → queue
/job:analyze all      # Assess fit → cover letter → save application
```

## Structure

```
jobs/
├── profile/
│   ├── profile.txt              # Skills, experience, projects
│   └── cover_letter_style.md    # Cover letter guidelines
├── sources.txt                  # Job board URLs to scrape
├── urls.txt                     # Seen URLs (dedup)
├── queue/                       # Jobs awaiting analysis
│   └── {company}_{role}.md
└── applications/                # Analyzed jobs with cover letters
    └── {company}_{role}.md
```

## Sources

- [Remote OK](https://remoteok.com) (JSON API)
- [We Work Remotely](https://weworkremotely.com)
- [Remotive](https://remotive.com) (JSON API)
- LinkedIn Jobs (via Playwright)
- Indeed (via Playwright)

## How It Works

**Scraping** (`/job:scrape`):
1. Reads `jobs/sources.txt` for board URLs
2. Fetches listings (Playwright for auth-required sites, WebFetch for APIs)
3. Deduplicates against `jobs/urls.txt`
4. Filters by role title (software, engineer, developer, QA, etc.)
5. Saves qualifying jobs to `jobs/queue/`

**Analysis** (`/job:analyze`):
1. Reads job description from queue
2. Compares requirements against `jobs/profile/profile.txt`
3. Rates fit: Strong / Good / Stretch / Poor
4. Generates cover letter per `jobs/profile/cover_letter_style.md`
5. Saves complete application to `jobs/applications/`
