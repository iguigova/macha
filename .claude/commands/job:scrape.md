---
description: Scrape jobs from sources
allowed-tools: mcp__playwright__*, WebFetch, Read, Write, Grep, Bash
---

Scrape job listings and queue new ones for analysis.

**Steps:**

1. Read jobs/sources.txt (skip comment lines starting with #)

2. **Read last scrape timestamp**: Read `jobs/last_scrape` (ISO 8601 timestamp). If the file doesn't exist, treat as first run (no date filter).

3. **FETCH STRATEGY**: Fetch from ALL sources to maximize candidate pool:
   - Fetch everything each source provides (respect URL params like count=50, limit=20)
   - For sources without built-in limits, fetch up to 50 candidates per URL
   - Keep ALL jobs that pass date check + dedup + filter (never discard passing jobs)

4. For each source URL in sources.txt (skip lines starting with #), fetch candidates:
   - **linkedin.com**: Playwright MCP. Navigate to https://www.linkedin.com/jobs/search/?keywords=software+developer&f_WT=2&sortBy=DD, then also search "QA engineer remote". Use browser_navigate, then browser_snapshot to read listings. Extract: title, company, job URL. If login required, tell user to log in and retry.
   - **indeed.com**: Playwright MCP. Navigate to https://www.indeed.com/jobs?q=software+developer&l=remote&sort=date, then also search "QA engineer remote". Use browser_navigate, then browser_snapshot to read listings. Extract: title, company, job URL. If login required, tell user to log in and retry.
   - **remoteok.com/api**: WebFetch, JSON array (first element is metadata, skip it). Fields: position, company, url, description. **Date field:** `epoch` (unix timestamp) or `date` (ISO string)
   - **remotive.com/api**: WebFetch, JSON object with `jobs` array. Fields: title, company_name, url, description. **Date field:** `publication_date` (ISO string)
   - **jobicy.com/api**: WebFetch, JSON object with `jobs` array. Fields: jobTitle, companyName, url, jobDescription. **Date field:** `pubDate` (ISO string)
   - **workingnomads.com/api**: WebFetch, JSON array. Fields: title, company_name, url, description, category_name. **Date field:** `pub_date` (ISO string)
   - **weworkremotely.com**: WebFetch, parse HTML for job listings. Extract title, company, and job URL from listing links. No date filtering (use dedup only).
   - **shopify.com/careers**: WebFetch, parse HTML. Look for job titles with "Apply" links. Company is "Shopify". Only keep roles marked "Remote". No date filtering.
   - **jobs.ashbyhq.com/{company}**: WebFetch, parse HTML. Each listing has a title and department. Extract title and link. Company name from URL path segment. Only keep roles mentioning "Remote" in location. No date filtering.
   - **Other career pages**: WebFetch, parse HTML for job listings. Extract role title and application URL. Company name comes from the source comment or page title. No date filtering.
   - For sources with URL parameters (count, limit, tag, category), use the URL as-is
   - Multiple URLs for the same API (e.g., different Jobicy tags) are intentional - fetch each separately

5. **DATE FILTER** (JSON APIs only): If `jobs/last_scrape` exists, skip any entry whose publication date is older than the last scrape timestamp. This avoids reprocessing entries from previous runs. Only applies to sources with a date field (remoteok, remotive, jobicy, workingnomads). HTML sources skip this step.

6. **DEDUPE**: Use the Grep tool (ripgrep) to check each remaining job against jobs/seen.txt (by company+role key)
   - Generate key: sanitized {company}_{role} (same as filename)
   - Grep for the key in seen.txt - skip if found

7. **THEN FILTER**: Keep only titles containing (case-insensitive):
   - software, developer, backend, frontend, fullstack
   - QA, test, quality
   - Skip: manager, director, designer, data scientist, ML, machine learning, devops

8. For each new job passing all checks:
   - Fetch full description
   - Save to jobs/queue/{company}_{role}.md:
     ```
     # {Company} - {Role}

     **URL:** {url}

     ## Description
     {job description}
     ```
   - Append key to jobs/seen.txt

9. **Update last scrape timestamp**: Write the current ISO 8601 timestamp (UTC) to `jobs/last_scrape`, overwriting the previous value.

10. Report stats:
   - Fetched: total candidates retrieved from sources
   - Skipped (old): entries older than last scrape (JSON APIs only)
   - Deduped: skipped (company+role already in seen.txt)
   - Filtered: dropped (title didn't match)
   - Queued: saved to jobs/queue/
   - Format: "Queued X jobs (fetched Y, old Z, deduped D, filtered F)"
   - List each queued job: `- {company}: {role}`

11. Log to session history: Append the report (from step 10) to `.claude/session_history.md` under a new dated heading `## {date} /job:scrape`.

**File naming:** Sanitize company/role names - lowercase, replace spaces with underscores, remove special chars.
