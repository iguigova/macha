---
description: Validate sources, discover new ones, update scrape command
allowed-tools: WebFetch, WebSearch, Read, Write, Edit, Bash, Grep, Glob, mcp__playwright__*
---

Validate existing job sources, discover new ones (including company career pages), and update the scrape command.

**Steps:**

## 1. Read current sources

Read jobs/sources.txt (skip comment lines starting with #).

## 2. Validate each source

For each source URL, fetch it and check ALL of the following:
- **Responds**: Does it return HTTP 200? (or valid redirect)
- **Has jobs**: Does the response contain at least 1 job listing?
- **Fresh**: Is the most recent job posted within the last 14 days?
- **Effective**: Do the returned job titles contain ANY of our filter keywords?
  - Match keywords (case-insensitive): software, developer, backend, frontend, fullstack, QA, test, quality
  - A source is effective if at least 5 of the returned jobs match these keywords
  - A source returning only irrelevant titles (drivers, interns, accountants, clinical, etc.) is NOT effective

**Fetch method by source type:**
- **JSON APIs** (remoteok.com, remotive.com, jobicy.com, workingnomads.com, etc.): Use WebFetch, parse JSON, check job titles
- **HTML pages** (weworkremotely.com, shopify.com, ashbyhq.com): Use WebFetch, parse HTML, check job titles
- **Playwright sources** (linkedin.com, indeed.com): Use Playwright MCP. Navigate to the URL with browser_navigate, then use browser_snapshot to read listings. Check job titles for relevance. If login is required, inform user and mark as NEEDS_LOGIN (do not remove).

**Classification:**
- **ACTIVE**: responds + has jobs + fresh + effective → keep
- **NEEDS_LOGIN**: Playwright source that requires authentication → keep, note in report
- **INACTIVE**: anything else (dead, empty, stale, irrelevant titles) → remove

## 3. Remove inactive sources

Delete all INACTIVE source URLs from jobs/sources.txt. Do not comment them out - remove them entirely. Keep the comment structure for remaining active sources.

## 4. Discover new job board sources

Search the web for:
- "remote software developer jobs API free"
- "remote QA jobs board API"
- "remote developer jobs JSON endpoint"
- "new job board API 2026"

For each candidate:
- Verify it returns job data (fetch and check)
- Check it's not already in sources.txt
- Confirm it's free (no API key required)
- Check effectiveness: does it return titles matching our filter keywords?
- Determine the correct URL with parameters for remote dev/QA jobs
- Add to sources.txt with a descriptive comment

## 5. Discover company career pages

Search the web for:
- "hiring remote software developers 2026"
- "remote software engineer careers page"
- "companies hiring remote developers Canada"
- "remote QA engineer openings"

For each company found:
- Find their careers/jobs page URL
- Verify the page lists current openings (fetch with WebFetch or Playwright as needed)
- Confirm they have relevant roles matching our filter keywords
- Prefer pages that WebFetch can parse (Ashby, Greenhouse board pages). Note: Lever pages require Playwright.
- Add to sources.txt under a `# Company career pages` section
- Format: direct URL to their filtered careers page (remote + engineering if possible)

## 6. Update the scrape command

Read .claude/commands/job:scrape.md and update step 3 (source handling) to include instructions for any newly added source types. For each new source, add:
- Domain pattern for matching
- Fetch method (WebFetch JSON, WebFetch HTML, or Playwright)
- Response structure (field names for title, company, url, description)
- Any special handling notes

## 7. Report

```
Source Report
=============
ACTIVE:     X sources kept
NEEDS_LOGIN: X sources need authentication
REMOVED:    X inactive/ineffective sources
NEW APIs:   X job board APIs added
NEW COs:    X company career pages added
TOTAL:      X sources in sources.txt

Active sources:
- {url}: ~N relevant jobs (examples: "Title 1", "Title 2")
- ...

Needs login:
- {url}: requires manual authentication in browser

Removed (reason):
- {url}: {reason - e.g., "0 jobs", "no relevant titles", "stale"}
- ...
```

## 8. Log to session history

Append the report (from step 7) to `.claude/session_history.md` under a new dated heading `## {date} /job:source`.
