---
description: Scrape jobs from sources
allowed-tools: mcp__playwright__*, WebFetch, Read, Write, Grep, Bash
---

Scrape job listings and queue new ones for analysis.

**Input:** $ARGUMENTS = minimum number of jobs to queue (default: 50)

**Steps:**

1. Read jobs/sources.txt

2. **CALCULATE FETCH SIZE**: Not all candidates pass dedup + filter, so overfetch to guarantee at least target jobs queued:
   - Target = $ARGUMENTS (minimum jobs to queue)
   - Fetch multiplier = 3x (accounts for ~40% filter dropout + dedup losses)
   - Candidates per source = ceil(target * 3 / number_of_active_sources)
   - Example: target=50, 3 API sources → fetch ~50 candidates per source
   - Keep ALL jobs that pass dedup + filter (never discard passing jobs)

3. For each source, fetch candidates (up to per-source limit from step 2):
   - LinkedIn/Indeed: Playwright MCP (requires authenticated browser)
   - Remote OK: WebFetch to https://remoteok.com/api (returns JSON)
   - We Work Remotely: WebFetch (parse HTML)
   - Remotive: WebFetch to https://remotive.com/api/remote-jobs (returns JSON)

4. **DEDUPE FIRST**: Check each job against jobs/seen.txt (by company+role key)
   - Generate key: sanitized {company}_{role} (same as filename)
   - Skip if key already exists in seen.txt

5. **THEN FILTER**: Keep only titles containing (case-insensitive):
   - software, developer, backend, frontend, fullstack
   - QA, test, quality
   - Skip: manager, director, designer, data scientist, ML, machine learning, devops

6. For each new job passing both checks:
   - Fetch full description
   - Save to jobs/queue/{company}_{role}.md:
     ```
     # {Company} - {Role}

     **URL:** {url}

     ## Description
     {job description}
     ```
   - Append key to jobs/seen.txt

7. Report stats:
   - Fetched: total candidates retrieved from sources
   - Deduped: skipped (company+role already in seen.txt)
   - Filtered: dropped (title didn't match)
   - Queued: saved to jobs/queue/
   - Format: "Queued X jobs (fetched Y, deduped Z, filtered W)"

**File naming:** Sanitize company/role names - lowercase, replace spaces with underscores, remove special chars.
