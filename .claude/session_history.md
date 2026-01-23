# Session History

## 2026-01-22 (source validation + Playwright + effectiveness filter)

**Done:**
- Ran `/job:source` command end-to-end, validated all 14 sources
- Removed 6 inactive/ineffective sources: Remotive QA (empty), Jobicy fullstack (stale), Jobicy geo=canada x2 (empty), Arbeitnow (irrelevant titles), Himalayas (irrelevant titles)
- Added 3 career pages: Shopify, Wrapbook, AuditBoard (all verified with remote engineering roles in Canada/Americas)
- Configured Playwright MCP (`@microsoft/playwright-mcp`) for LinkedIn/Indeed scraping
- Updated `/job:source` command: added `mcp__playwright__*` to allowed-tools, added "Effective" check (5+ titles must match filter keywords), added NEEDS_LOGIN classification for Playwright sources, extended freshness to 14 days
- Updated `/job:scrape` command: added specific Playwright instructions for LinkedIn/Indeed (navigate + snapshot), removed Arbeitnow/Himalayas handling, added Shopify/Ashby career page handling
- Updated scrape command dedup step to explicitly use Grep tool (ripgrep)
- Updated README and JOB_PIPELINE_PLAN.md to match

**Suggested commit:**
```
Run /job:source, add effectiveness filter, configure Playwright MCP

Validated all sources: removed 6 inactive/ineffective (Arbeitnow,
Himalayas returned irrelevant titles; Remotive QA empty; Jobicy
fullstack stale; Jobicy geo=canada empty). Added 3 career pages
(Shopify, Wrapbook, AuditBoard). Source command now checks that 5+
returned titles match filter keywords, uses Playwright for LinkedIn/
Indeed validation, and classifies auth-required sources as NEEDS_LOGIN.
Configured @microsoft/playwright-mcp for authenticated scraping.
```

---

## 2026-01-22 (sources overhaul)

**Done:**
- Researched and verified 6 free JSON APIs for remote job scraping (RemoteOK, Remotive, Jobicy, Arbeitnow, Working Nomads, Himalayas)
- Expanded jobs/sources.txt from 5 URLs to 14 (with comments and parameters)
- Created `/job:source` command (validate, prune inactive, discover APIs + career pages, update scrape command)
- Removed argument from `/job:scrape` (fetches all sources, no target number)
- Updated JOB_PIPELINE_PLAN.md with new architecture (5 commands)
- Updated README with new workflow and sources

**Suggested commit:**
```
Rename /job:sources to /job:source, remove scrape argument, add career page discovery

Source command now always validates+discovers (no args), removes inactive
sources entirely, discovers company career pages, and updates the scrape
command. Scrape command fetches all sources without a target number.
Updated JOB_PIPELINE_PLAN.md and README to reflect new workflow.
```

---

## 2026-01-22 13:31

**Done:**
- Fixed session history filename typo (sessin_history.md → session_history.md)
- Moved session history from log.md to .claude/session_history.md (standard location)

**Decisions:**
- Use standard .claude/session_history.md path for /memento compatibility

**State:**
- Session history now in correct location
- Job pipeline commands ready in .claude/commands/

**Next:**
- Create profile/cover_letter_style.md
- Test /job-scrape
- Process jobs with /job-analyze

---

## 2026-01-22 13:28

**Done:**
- Verified JOB_PIPELINE_PLAN.md implementation status
- Moved `/job-scrape` and `/job-analyze` commands from global (~/.claude/commands/) to project-local (.claude/commands/)
- Removed hardcoded working directory from commands (now uses project context)

**Decisions:**
- Keep job pipeline commands local to macha project (not global)
- Session history stays in log.md (not .claude/session_history.md)

**State:**
- Job pipeline fully implemented: commands, directory structure, sources.txt all in place
- Missing only profile/cover_letter_style.md
- Commands `/job-scrape` and `/job-analyze` ready to use

**Next:**
- Create profile/cover_letter_style.md
- Test `/job-scrape` with live sources
- Process queued jobs with `/job-analyze`

---

## 2026-01-22 09:49

**Done:**
- Created full directory structure
- Moved all profile files (v3, v4, v5, cover_letter_style.md)
- Created CLAUDE.md with project guidelines and learnings structure
- Created interview_cheatsheet.md with STAR examples and QA prep from ChatGPT
- Created jobs/tracker.md with de-dup system
- Extracted ChatGPT conversations to profile/resources/
- First scrape: ~35 jobs from Remote OK + We Work Remotely
- Saved scraped jobs to jobs/scraped/2026-01-22_initial_scrape.md
- Identified high-priority matches (Java, .NET, Backend roles)

**Decisions:**
- Remote OK has JSON API - easiest to automate
- We Work Remotely has clean HTML - good for scraping
- Created priority ranking: Strong fit → Good fit → QA positions

**State:**
- All setup complete
- 35 jobs scraped and ready for review
- 4 strong-fit positions identified (Java/C#/.NET stack)
- 2 QA positions available

**Next:**
- User to rate first batch of jobs
- Generate cover letters for top-rated
- Submit first applications
- More scraping from Indeed, LinkedIn, Wellfound

---

## 2026-01-22 09:41

**Done:**
- Created directory structure: profile/, jobs/, .claude/, automation/
- Copied profile files from Downloads (v3, v4, v5, cover_letter_style.md)
- Extracted ChatGPT conversations from export zip (PlanHub fit, micro1 QA interview)
- Drafted job search automation plan

**Decisions:**
- Using devnull-style memento pattern for session logging
- Target: 50-100 applications per day (numbers game)
- Position as quick learner, open to any role/stack/level
- Remote only requirement
- FTE or contract both acceptable

**State:**
- Profile organized in macha/profile/
- ChatGPT conversations extracted (valuable interview prep content)
- Plan documented in ~/.claude/plans/cosmic-wobbling-aurora.md
- Ready to create interview cheatsheet and job tracker

**Next:**
- Create CLAUDE.md with project guidelines
- Create interview_cheatsheet.md from profile + ChatGPT content
- Create job tracker with de-duplication
- First scraping test
