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
   - **remoteok.com/api**: WebFetch, JSON array (first element is metadata, skip it). Extract ALL fields: position, company, url, **description**, epoch, date. **Date field:** `epoch` (unix timestamp) or `date` (ISO string)
   - **remotive.com/api**: WebFetch, JSON object with `jobs` array. Extract ALL fields: title, company_name, url, **description**, publication_date. **Date field:** `publication_date` (ISO string)
   - **jobicy.com/api**: WebFetch, JSON object with `jobs` array. Extract ALL fields: jobTitle, companyName, url, **jobDescription**, pubDate. **Date field:** `pubDate` (ISO string)
   - **workingnomads.com/api**: WebFetch, JSON array. Extract ALL fields: title, company_name, url, **description**, category_name, pub_date. **Date field:** `pub_date` (ISO string)
   - **weworkremotely.com**: WebFetch, parse HTML for job listings. Extract title, company, and job URL from listing links. No date filtering (use dedup only).
   - **shopify.com/careers**: WebFetch, parse HTML. Look for job titles with "Apply" links. Company is "Shopify". Only keep roles marked "Remote". No date filtering.
   - **jobs.ashbyhq.com/{company}**: WebFetch, parse HTML. Each listing has a title and department. Extract title and link. Company name from URL path segment. Only keep roles mentioning "Remote" in location. No date filtering.
   - **Other career pages**: WebFetch, parse HTML for job listings. Extract role title and application URL. Company name comes from the source comment or page title. No date filtering.
   - For sources with URL parameters (count, limit, tag, category), use the URL as-is
   - Multiple URLs for the same API (e.g., different Jobicy tags) are intentional - fetch each separately

5. **DATE FILTER** (JSON APIs only): If `jobs/last_scrape` exists, skip any entry whose publication date is older than the last scrape timestamp. This avoids reprocessing entries from previous runs. Only applies to sources with a date field (remoteok, remotive, jobicy, workingnomads). HTML sources skip this step.

6. **DEDUPE** (two-phase):
   a. **Within-batch**: Collect all candidates from all sources into a single list keyed by sanitized company_role. If the same key appears from multiple sources, keep the first occurrence and discard duplicates.
   b. **Against seen.txt**: Use the Grep tool (ripgrep) to check each remaining key against jobs/seen.txt. Skip if found.

7. **THEN FILTER**: Normalize each title by removing hyphens, then apply case-insensitive substring matching.
   - Keep titles containing: software, developer, backend, frontend, fullstack, QA, test, quality, sdet
   - Skip titles containing: manager, director, designer, data scientist, ML, machine learning, devops

8. For each new job passing all checks:
   - **Get description**:
     - JSON API jobs (remoteok, remotive, jobicy, workingnomads): use the description already extracted from the API response in step 4
     - HTML source jobs (weworkremotely, shopify, ashbyhq, other career pages): batch-fetch individual job page URLs in parallel groups (~9 concurrent WebFetch calls per batch). Extract full job description, requirements, and qualifications from each page.
   - Save to jobs/queue/{company}_{role}.md:
     ```
     # {Company} - {Role}

     **URL:** {url}

     ## Description
     {full job description}
     ```
   - Append key to jobs/seen.txt

9. **Update last scrape timestamp**: Write the current ISO 8601 timestamp (UTC) to `jobs/last_scrape`, overwriting the previous value.

10. Report stats:
   - Fetched: total candidates retrieved from sources
   - Skipped (old): entries older than last scrape (JSON APIs only)
   - Deduped (batch): cross-source duplicates within this batch
   - Deduped (seen): already in seen.txt from previous runs
   - Filtered: dropped (title didn't match)
   - Queued: saved to jobs/queue/
   - Format: "Queued X jobs (fetched Y, old Z, deduped-batch B, deduped-seen S, filtered F)"
   - List each queued job: `- {company}: {role}`

11. Log to session history: Append the report (from step 10) to `.claude/session_history.md` under a new dated heading `## {date} /job:scrape`.

**File naming:** Sanitize company/role names - lowercase, replace spaces with underscores, remove special chars.
