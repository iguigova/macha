---
description: Scrape jobs from sources
allowed-tools: mcp__playwright__*, WebFetch, Read, Write, Grep, Bash
---

Scrape job listings and queue new ones for analysis.

**Input:** $ARGUMENTS = number of jobs (default: 50)

**Steps:**

1. Read jobs/sources.txt

2. For each source, fetch job listings:
   - LinkedIn/Indeed: Playwright MCP (requires authenticated browser)
   - Remote OK: WebFetch to https://remoteok.com/api (returns JSON)
   - We Work Remotely: WebFetch (parse HTML)
   - Remotive: WebFetch to https://remotive.com/api/remote-jobs (returns JSON)

3. **DEDUPE FIRST**: Check each URL against jobs/urls.txt
   - Skip if URL already exists

4. **THEN FILTER**: Keep only titles containing (case-insensitive):
   - software, engineer, developer, backend, frontend, fullstack
   - QA, SDET, test, quality
   - Skip: manager, director, designer, data scientist, ML, machine learning

5. For each new job passing both checks:
   - Fetch full description
   - Save to jobs/queue/{company}_{role}.md:
     ```
     # {Company} - {Role}

     **URL:** {url}

     ## Description
     {job description}
     ```
   - Append URL to jobs/urls.txt

6. Report: "Added X jobs to queue"

**File naming:** Sanitize company/role names - lowercase, replace spaces with underscores, remove special chars.
