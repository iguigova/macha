# Session History

## 2026-02-22 Cache exclusion list as URL hashes

Updated `/job:apply` step 3 to use a `jobs/done/.exclusions` cache file (one SHA-256 hash per line) instead of re-reading all done files every invocation. Cache is rebuilt from `*.md` files if missing. Step 12 appends new hashes on successful application.

## 2026-02-18 /job:apply (run 2)

**Applied: 1, Skipped: 0, Failed: 0**
- Grafana Labs: Senior Backend Engineer, Mimir — **applied**
  - URL: https://job-boards.greenhouse.io/grafanalabs/jobs/5799745004
  - Canada Remote, CAD 146,000–175,000
  - Tech: Go, Kubernetes, distributed systems, observability (Mimir/Prometheus)
  - Fit: Go experience, Kubernetes 3+ years, microservices architecture, distributed systems. Posting accepts adjacent language backgrounds (C, C++).
  - Source: WebSearch → Greenhouse job board

**New facts learned: 3**
- Updated Go experience from 1 year to 5+ years per user correction
- Added observability space experience fact per user correction
- Updated disability status from "Prefer not to answer" to "No" per user correction

**Suggested commit:**
```
Apply to Grafana Labs Senior Backend Engineer Mimir, update profile facts

Submit application via Greenhouse. Update profile: Go experience to 5+
years, add observability experience, disability status to No — all per
user corrections.
```

---

## 2026-02-18 /job:apply

**Applied: 1, Skipped: 0, Failed: 0**
- Coconut Software: Senior Developer (12 Month Contract) — **applied**
  - URL: https://job-boards.greenhouse.io/coconutsoftware/jobs/5783875004
  - Canada Remote, $110K-$140K CAD, 12-month contract
  - Tech: React, TypeScript, PHP/Laravel, PostgreSQL/MariaDB, Docker, AWS, RESTful
  - Fit: Strong on React, TypeScript, relational DBs, REST APIs, Docker, AWS, CI/CD, system design. Gap: PHP/Laravel.
  - Source: LinkedIn (remote software engineer, Canada, sorted by most recent)

**New facts learned: 7**
- 6 new writing voice rules added to profile.txt (no jargon labels, no closing lines, no positioning statements, personality answers genuine and brief, each sentence a fact not a pitch, screening answers state what you built)
- 1 correction: user is NOT a motorcyclist

**Suggested commit:**
```
Apply to Coconut Software Senior Developer, update writing voice rules

Submit application via Greenhouse. Add 6 writing voice rules to profile:
no jargon labels, no try-hard closing lines, no positioning statements,
genuine personality answers, each sentence a fact not a pitch. Answers
refined through 4 rounds of user feedback.
```

---

## 2026-02-17 Simplify /job:apply Phase 1 (Find)

**Done:**
- Replaced source ladder + fit rubric (steps 3-7) with simpler steps 3-5
- No rigid tiers — use any source, start where results are most likely
- LinkedIn promoted to first-listed source (was buried in Tier 3)
- Removed 5-point fit rubric — replaced with 1-sentence "Why" per job
- Email-apply jobs now accepted (was rejected for lacking ATS forms)
- "Remote US" explicitly marked as fine (many hire Canadians)
- Renumbered Phase 2 steps from 8-17 to 6-15
- Updated README.md Phase 1 description to match

**Suggested commit:**
```
Simplify /job:apply Phase 1: drop source ladder and fit rubric

Replace rigid 3-tier source ladder and 5-point fit rubric with flexible
source list and bias-toward-keeping rules. LinkedIn no longer buried in
Tier 3. Email-apply and "Remote US" jobs now accepted. Results show a
fit sentence instead of numeric scores. Phase 2 unchanged (renumbered).
```

---

## 2026-02-16 /job:apply (source ladder test)

**Applied: 0, Skipped: 1, Failed: 0**
- Fortis Games: Staff Backend Engineer, Game Team — **skipped** (user declined)
  - URL: https://boards.greenhouse.io/fortisgames/jobs/4646027005
  - Remote Canada, C#/.NET, AWS, Unity, CI/CD
  - Score: 3/5 Good: +lang(C#/.NET) +seniority(Staff) +infra(AWS, CI/CD) +location(Canada) −gap(Unity/games)
  - Source: Tier 1 → Jobicy API (backend tag)

**Source ladder performance:**
- Tier 1 sources tried: HN Hiring (Go, TypeScript), Jobicy API (backend)
- HN Go: 3 passed filter gate but all dropped on deeper inspection (Stream = Amsterdam only, AllSpice = no open roles, Shinzo = blockchain)
- HN TypeScript: 4 passed filter gate but none had ATS URLs (Lift AI = email only, BetterDB = email only, Evervault = Dublin/London only, SpruceID = couldn't load Lever page)
- Jobicy backend: Fortis Games found (Canada, Greenhouse URL, 3/5 Good)
- Total fetches: ~14 (too many — Stream/AllSpice/Evervault/SpruceID fetches were wasted on dead ends)

**Observations:**
- Many HN postings are email-only (no ATS form) — Phase 2 can't handle these
- Need to prioritize candidates with ATS URLs earlier in the pipeline
- Game industry roles are the main Canada-remote C# jobs right now

**New facts learned: 0**

---

## 2026-02-16 Redesign /job:apply Phase 1 (Find)

**Done:**
- Replaced ad-hoc search (steps 3-4) with deterministic 3-tier source ladder
- Tier 1: Structured sources (HN Hiring, Jobicy API, RemoteOK API, Ashby pages) — 1 fetch = many jobs
- Tier 2: WebSearch with strict rules (no year, no "Canada", use site: operators)
- Tier 3: Playwright (LinkedIn, WeWorkRemotely, Working Nomads) — last resort
- Added filter gate (skip before fetching full description)
- Added 5-point fit rubric with score breakdown in results presentation
- Removed dead sources (Remotive API → 410 errors)
- Renumbered Phase 2 steps (8-17)

**Suggested commit:**
```
Redesign /job:apply Phase 1 with source ladder + fit rubric

Replace ad-hoc job search with deterministic 3-tier source ladder:
structured APIs first (HN Hiring, Jobicy, RemoteOK, Ashby), then
WebSearch with strict query rules, then Playwright as last resort.
Add filter gate (skip before fetch) and 5-point fit rubric with
score breakdown. Remove dead Remotive API. Phase 2 unchanged.
```

---

## 2026-02-16 /job:apply

**Applied: 0, Skipped: 0, Failed: 1**
- Found: Staff Software Engineer, Platform — **failed: Playwright MCP not connected**
  - URL: https://jobs.ashbyhq.com/found/9d31df05-8818-49b3-9647-d492ce1f2a1f
  - Remote Canada, $240K-$278K CAD
  - Tech: Ruby on Rails, React/TypeScript, PostgreSQL, AWS, Kubernetes
  - Fit: Strong on backend infrastructure, SOC2/PA-DSS compliance, AWS/Docker/K8s, CI/CD. Platform/infra role aligns with career history of building systems other teams depend on.
  - Source: HN Who is Hiring (Feb 2026) → Ashby job board
  - Status: PENDING — form data and screening answer saved to jobs/done/found_staff_software_engineer_platform.md, ready to submit when Playwright available

**New facts learned: 2**
- Writing voice: Do not open with sweeping career narratives — lead with concrete work
- Writing voice: Do not frame the company's stage or opportunity back at them

**Suggested commit:**
```
Prepare Found Staff SWE Platform application, update writing voice

Save application data for Found Staff Software Engineer (Platform) to
jobs/done/. Add two writing voice rules to profile.txt: no sweeping
career narratives as openers, no framing the company's stage back at
them. Playwright MCP not connected — submission pending.
```

---

## 2026-02-15 /job:apply

**Applied: 1, Skipped: 0, Failed: 0**
- Metabase: Software Engineer (Backend) — **applied**
  - URL: https://jobs.lever.co/metabase/85f454d8-e795-4978-8a2b-4b8bfa7d7c37
  - Global Remote, fully distributed (50% outside US)
  - Tech: Clojure, JavaScript/TypeScript, JDBC, JVM
  - Fit: Strong on JVM/Java, JS/TS, relational databases, distributed teams, open source. Gap: Clojure is personal projects only.
  - Source: WebSearch → Lever job board

**New facts learned: 1**
- Updated Java/Kotlin experience from "10+ years" to "4+ years" per user correction

**Suggested commit:**
```
Apply to Metabase Software Engineer (Backend)

Record application to Metabase via /job:apply. Update Java/Kotlin
experience fact in profile.txt from "10+ years" to "4+ years" per
user correction. Cover letter refined through two rounds of user
feedback — removed technical namedropping, tightened language.
```

---

## 2026-02-14 Streamline /job:apply and unify profile voice

**Done:**
- Unified cover letter style + screening tone into single "Writing voice" block in profile.txt (lines 109-119)
- Merged steps 4 (fit assessment) and 5 (filter mismatches) into step 3b as skip criteria during search
- Made cover letter generation conditional — only if form has a cover letter field (textarea or file upload)
- Added PDF cover letter generation via `soffice --headless --convert-to pdf` for file upload fields
- Changed screenshot path to `jobs/done/{company_role}_form.png`
- Expanded done report to capture: form data with sources, facts used, facts learned, corrections applied
- Renumbered all steps (17→15 total)

**Suggested commit:**
```
Streamline /job:apply pipeline and unify profile voice

Merge fit assessment and mismatch filtering into search step (skip
criteria during search, not separate passes). Unify cover letter style
and screening tone into single "Writing voice" block in profile.txt.
Make cover letter conditional (only if form requires it, PDF via
soffice for file uploads). Save screenshots to jobs/done/. Expand
done report with form data sources, facts used, and corrections.
```

---

## 2026-02-14 Improve /job:apply search, dedup, tone

**Done:**
- Removed "Job Search Parameters" section from CLAUDE.md (redundant — /job:apply derives everything from profile.txt)
- Added 3 tone calibration facts to profile.txt (screening question voice, no pitch-back closing lines, "helped build" for team work)
- Rewrote /job:apply steps 2-4: merged search + dedup into unified find-and-filter flow with exclusion list, early filtering, and pacing (sequential for N≤3, parallel for N>3)
- Renumbered all steps (18→17 total)

**Suggested commit:**
```
Improve /job:apply search, dedup, and tone calibration

Remove redundant Job Search Parameters from CLAUDE.md. Add screening
question tone facts to profile.txt. Merge search and dedup into unified
step with exclusion list (filename + URL), early filtering, and pacing
(sequential for N≤3, parallel for N>3).
```

---

## 2026-02-14 /job:apply

**Applied: 1, Skipped: 0, Failed: 0**
- Coder: Senior Software Engineer (AI Tools) — **applied**
  - URL: https://jobs.ashbyhq.com/Coder/1a767f93-8f59-4038-bb1c-8c7e93165bed
  - Remote US/Canada, CA$150K–CA$202K + equity + bonus
  - Tech: Go, TypeScript, React, Postgres, Kubernetes, GitHub Actions
  - Fit: Strong on full-stack experience, TypeScript/React, Kotlin, Postgres, K8s, CI/CD. Gap: 1yr Go vs 3+ required.
  - Source: Hacker News Who is Hiring (February 2026)

**New facts learned: 1**
- Updated race/ethnicity from "Prefer not to answer" to "White / Caucasian"

**Suggested commit:**
```
Apply to Coder Senior Software Engineer (AI Tools)

Record application to Coder via /job:apply. Update race/ethnicity
fact in profile.txt from "Prefer not to answer" to "White / Caucasian".
```

---

## 2026-02-13 Pipeline simplification

**Done:**
- Collapsed 5 commands (source, scrape, analyze, answer, apply) into 1: `/job:apply N`
- Flattened `profile.txt` — removed all `#` section headers, now pure flat facts (115 lines)
- Rewrote `.claude/commands/job:apply.md` — two-phase flow: Find (search + assess fit) → Apply (fill + pause + learn + submit)
- Deleted: `job:source.md`, `job:scrape.md`, `job:analyze.md`, `job:answer.md`
- Deleted: `sources.txt`, `sources.bak`, `seen.txt`, `last_scrape`, `JOB_PIPELINE_PLAN.md`
- Deleted: `jobs/queue/`, `jobs/applications/`, `jobs/discarded/` directories
- Updated `CLAUDE.md` and `README.md` to reflect single-command pipeline

**Design:**
- `/job:apply N` finds N jobs, assesses fit, applies with human-in-the-loop (pause before every submit)
- `/job:apply URL` applies directly to a specific job
- Learning loop: user corrections become new facts in profile.txt
- Dedup by checking `jobs/done/` filenames
- All data derived from flat facts on demand (cover letters, screening answers, form data)

**Suggested commit:**
```
Simplify pipeline to single /job:apply command

Collapse 5 commands (source, scrape, analyze, answer, apply) into one.
Two-phase flow: Find (search any source, assess fit) then Apply (fill
form, pause for user review, learn from corrections). Flatten profile.txt
to pure facts (no section headers). Delete all intermediate pipeline
files and directories. Profile grows smarter with every application.
```

---

## 2026-02-06 /job:source

```
Source Report
=============
ACTIVE:      9 sources kept
NEEDS_LOGIN: 2 sources need authentication
REMOVED:     5 inactive/ineffective sources
NEW APIs:    0 (discovery skipped)
NEW COs:     0 (discovery skipped)
TOTAL:       11 sources in sources.txt

Active sources:
- remoteok.com/api: ~20 jobs, 7 relevant ("Software Development Engineer", "Senior Software Engineer Web")
- remotive.com/api: ~4 jobs, 4 relevant ("Senior Python Backend Developer", "Full Stack Sr/Staff Software Engineer")
- jobicy.com tag=developer: 8 jobs, 8 relevant ("Sr. Full Stack Developer", "Kotlin Game Developer")
- jobicy.com tag=software: 21 jobs, 16 relevant ("Senior Software Engineer (backend)", "Senior Software Engineer, New Product")
- jobicy.com tag=frontend: 5 jobs, 5 relevant ("Sr Frontend Engineer", "Staff Frontend Engineer")
- jobicy.com tag=backend: 9 jobs, 9 relevant ("Senior Software Engineer (backend)", "Backend Engineer, Database Excellence")
- workingnomads.com/api: 40 jobs, 26 relevant ("Senior Java & React Developer", "Senior Backend Developer")
- weworkremotely.com: 42 jobs, 26 relevant ("Senior Java Full-stack Developer", "Senior Software Engineer")
- jobs.ashbyhq.com/auditboard: 60 jobs, 5+ relevant ("Senior Software Engineer II Full Stack", "Staff Software Engineer Architecture")

Needs login:
- linkedin.com/jobs: requires manual authentication in browser
- indeed.com/jobs: requires manual authentication in browser

Removed:
- jobicy.com tag=qa: only 2 jobs (below 5 threshold)
- jobs.ashbyhq.com/wrapbook: only 3 relevant out of 19 (below 5 threshold)
- jobs.ashbyhq.com/cohere: only 2 relevant out of 64 (below 5 threshold)
- shopify.com/careers: not validated (fetch rejected)
- jobs.ashbyhq.com/fieldguide: not validated (fetch rejected)
- jobs.ashbyhq.com/tenex: not validated (fetch rejected)
```

---

## 2026-02-06 (fact-based profile + /job:answer + /job:apply)

**Done:**
- Rewrote `jobs/profile/profile.txt` as flat dump of ~120 natural language facts (was 476 lines with structured sections)
- Merged `cover_letter_style.md` into profile.txt as facts, deleted the separate file
- Created `.claude/commands/job:answer.md` — derives screening answers from profile facts, learns new facts from user
- Created `.claude/commands/job:apply.md` — Playwright browser automation to fill forms, upload resume, submit applications
- Updated `.claude/commands/job:analyze.md` — now selects 3-4 most relevant career facts per job instead of summarizing whole career
- Updated `JOB_PIPELINE_PLAN.md` — documented all 5 commands, updated file list, workflow, verification
- Updated `README.md` — new commands, workflow, structure, how-it-works sections

**Key design decisions:**
- Profile is a flat dump of natural language facts, no tags/metadata/structure — Claude infers relevance from content
- Single source of truth: all form data, screening answers, cover letters derived from same facts
- Learning loop: unknown answers → ask user → add fact → auto-answered next time
- Platform detection by URL pattern: LinkedIn, Indeed, Ashby, Greenhouse, Lever, Workday, generic

**Suggested commit:**
```
Add /job:apply, /job:answer commands and fact-based profile

Restructure profile.txt as flat natural language facts (~120 statements)
replacing 476-line structured document. Create /job:apply for browser
automation (Playwright MCP) across LinkedIn, Indeed, Ashby, Greenhouse,
Lever, Workday. Create /job:answer for screening question answers with
profile learning loop. Update /job:analyze to select 3-4 relevant facts
per job. Delete cover_letter_style.md (merged into profile).
```

---

## 2026-01-23 (permissions & settings consolidation)

**Done:**
- Added global instruction to `~/.claude/CLAUDE.md`: suggest commit message + log to session history after each prompt
- Consolidated all tool permissions into `~/.claude/settings.json` (global): Edit, Write, WebFetch domains, Bash patterns, Playwright tools
- Cleaned up `.claude/settings.local.json` (removed broken multi-line bash fragments, cleared duplicates)

**Suggested commit:**
```
Consolidate Claude Code permissions globally, add commit-per-prompt instruction

Move all tool permissions (WebFetch domains, Bash patterns, Playwright
tools) from project-local settings.local.json to global ~/.claude/settings.json.
Add Edit/Write to global allow list. Add instruction to always suggest
a commit message and log to session history after each prompt.
```

---

## 2026-01-23 /job:analyze

**Analyzed 29 jobs, saved to jobs/applications/**

79 jobs remain in queue (minimal descriptions — Jobicy/Remotive/WeWorkRemotely/Ashby pages that couldn't be fetched via WebFetch due to JS rendering or site blocks).

Jobs analyzed:
- ChurnZero: Full Stack Software Engineer (Strong fit)
- Higher Logic: Junior Software Developer (Strong fit)
- Netflix: Software Engineer 5 (Good fit)
- Netflix: Software Engineer - Open Connect Control Plane (Good fit)
- SpruceID: Full-Stack Software Engineer – Remote (Strong fit)
- Reddit: Junior Full-Stack Software Engineer (Strong fit)
- Pomelo Care: Software Engineer (Good fit)
- Crossing Hurdles: Software Engineer | $85/hr (Good fit)
- Crossing Hurdles: Junior Software Engineer (Strong fit)
- Flagler Health: Software Engineer - Frontend (Good fit)
- Hashnode: Fullstack Developer (Good fit)
- Socket: Software Engineer (Strong fit)
- Runpod: Software Engineer (Full-Stack) (Good fit)
- Ad Hoc Labs: Software Engineer - Web (Good fit)
- Jasper: Software Engineer - Backend (Good fit)
- ComboCurve: Full-Stack Developer (Stretch)
- Flex: Software Engineer I, Backend (Strong fit)
- Microsoft: Software Engineer (Good fit)
- Wealthfront: Software Engineer, Frontend (Good fit)
- Oracle: Software Developer (Strong fit)
- PathAI: Software Engineer, Full Stack (Stretch)
- PolicyMe: Junior Software Engineer (Good fit)
- Posit PBC: Software Engineer (Stretch)
- Shopify: Software Engineers (Good fit)
- Jerry: Software Engineer (entry) (Good fit)
- Clarvos: Junior Software Engineer (Good fit)
- hackajob: Junior Software Engineer (Poor fit — grad program, US auth required)
- Abridge: Software Engineer (Good fit — but HYBRID ONLY, skip)
- Actionable.co: Senior Front-End Developer (Stretch)

Summary: 8 Strong fit, 15 Good fit, 4 Stretch, 2 Skip (Poor fit / location mismatch)

---

## 2026-01-22 /job:scrape

**Queued 27 jobs** (fetched 60, old 0, deduped-batch 8, deduped-seen 2, filtered 23)

Source: LinkedIn (public, no login required for first page). Used Playwright to navigate to remote software developer search (f_WT=2 filter). Other sources commented out or removed from sources.txt (only linkedin.com/jobs active).

Note: sources.txt was modified to only contain LinkedIn. Full source list preserved in sources.bak.

Jobs queued:
- SpruceID: Full-Stack Software Engineer (New Grad) – Remote
- Crossing Hurdles: Software Engineer | $85/hr | Remote
- PolicyMe: Junior Software Engineer (Remote)
- Higher Logic: Junior Software Developer
- Posit PBC: Software Engineer
- Clarvos: Junior Software Engineer (Fresh Graduates)
- hackajob: Junior Software Engineer
- Jerry: Software Engineer (entry)
- ChurnZero: Full Stack Software Engineer
- Socket: Software Engineer
- Hashnode: Fullstack Developer (Remote)
- Crossing Hurdles: Junior Software Engineer | Remote
- Pomelo Care: Software Engineer (Remote)
- ComboCurve: Full-Stack Developer
- Flagler Health: Software Engineer - Frontend (Remote)
- Netflix: Software Engineer - Open Connect Control Plane
- Netflix: Software Engineer 5
- Abridge: Software Engineer (All Levels)
- Microsoft: Software Engineer
- Wealthfront: Software Engineer, Frontend
- PathAI: Software Engineer, Full Stack
- Runpod: Software Engineer (Full-Stack)
- Ad Hoc Labs: Software Engineer - Web
- Jasper: Software Engineer - Backend
- Reddit: Junior Full-Stack Software Engineer - Business Manager
- Oracle: Software Developer
- Flex: Software Engineer I, Backend (New Grad)

---

## 2026-01-23 /job:scrape

**Queued 1 jobs** (fetched 259, old 250, deduped-batch 0, deduped-seen 80, filtered 7)

All JSON API entries older than last_scrape (2026-01-23T02:53:07Z) - 0 new from APIs. HTML sources checked for new listings.

Jobs queued:
- Shopify: Software Engineers (core/always-hiring, Remote Americas)

Note: Most HTML source listings unchanged from previous scrape. LinkedIn/Indeed skipped (Playwright not available).

---

## 2026-01-22 (scrape command improvements)

**Done:**
- Fixed within-batch dedup: two-phase approach - first deduplicate cross-source overlaps by key within the batch, then check against seen.txt
- Fixed description fetching: JSON API sources now explicitly extract descriptions from API response; HTML sources batch-fetch individual job pages in parallel (~9 concurrent)
- Fixed title filter: normalize titles by removing hyphens before matching (full-stack→fullstack, front-end→frontend, back-end→backend). Added "sdet" keyword.
- Updated stats format to show batch vs seen dedup counts separately
- Updated JOB_PIPELINE_PLAN.md and README.md

**Suggested commit:**
```
Fix scrape command: two-phase dedup, description extraction, title normalization

Three fixes based on first full scrape run:
1. Dedup now handles cross-source duplicates within a batch before
   checking seen.txt (e.g. same job from multiple Jobicy tags)
2. JSON API descriptions extracted from API response; HTML source
   descriptions batch-fetched in parallel (~9 concurrent WebFetch)
3. Title filter normalizes hyphens (full-stack matches fullstack)
   and adds sdet keyword. Updated plan and README.
```

---

## 2026-01-22 /job:scrape

**Queued 80 jobs** (fetched 259, old 0, deduped 9, filtered 170)

Sources scraped: RemoteOK, Remotive, Jobicy (x5 tags), Working Nomads, WeWorkRemotely, Shopify, Wrapbook, AuditBoard, Fieldguide, TENEX.AI, Cohere (LinkedIn/Indeed skipped - Playwright not available)

Jobs queued:
- Evolve Tech LLC: Backend Developer
- A.Team: Senior Independent Software Developer
- Mitre Media: Tech Lead Full-Stack Rails Engineer
- Apexver: Software Engineer C++ (Senior)
- Actionable.co: Senior Front-End Developer
- Infor: .NET Developer Senior (AI integration)
- Infor: .Net Principal Developer (Multi-tenant Integration)
- Magic Media: Kotlin Game Developer
- Mindrift: Freelance Software Developer (Kotlin)
- Lumen Technologies: Senior Software Developer
- Eurofins: D365 FO/.NET Developer
- RemoteStar: Python Developer
- Nacre Capital: Full Stack Developer
- Mindrift: Freelance Software Developer (Java)
- Binance: Android Developer/Architect
- Trimble: Senior Software Developer – Fullstack
- ECS: Microsoft Dynamics Developer
- GiveDirectly: Senior Software Engineer
- Flex: Staff Software Engineer, Risk Engineering
- Kraken: Senior Software Engineer – Frontend – Onchain
- Tines: Software Engineer
- Autodesk: Principal Software Engineer
- Akamai: Software QA Automation Engineer
- Match Group: Senior Software Engineer, iOS
- GoDaddy: Principal Engineer, Software Development
- Motive: Staff Bluetooth Software Engineer
- Teachable: Software Engineer II (mid-level)
- TRM Labs: Senior Software Engineer, Data Platform
- HubSpot: Senior Software Engineer, Frontend, Fintech
- Socure: Senior Backend Engineer
- Canva: Backend Engineer, User Platform – Java
- Canva: Senior Backend Engineer, User Platform – Java
- Turnitin: Senior Software Engineer, Fullstack/Backend
- Turnitin: Software Engineer, Fullstack/Backend
- Proxify: Senior Ruby on Rails Developer
- Proxify: Senior Fullstack Developer (Vue.js & Python)
- Proxify: Senior Backend Developer (Node.js / Nest.js)
- Proxify: Senior Fullstack Developer (Python)
- Proxify: Senior Django Developer
- Proxify: Senior Backend Developer (Python)
- Proxify: Senior MS Power BI Developer
- Sticker Mule: Software Engineer
- SerpNames: Passionate Full-Stack Engineer
- Planning Center: Full Stack Engineer
- Zencastr: Senior Fullstack Developer
- fonio: Senior Software Engineer
- Stellar AI: Senior Software Engineer
- DynamiCare Health: Full-Stack Engineer
- Lemon.io: Senior PHP Full-stack Developer
- Lemon.io: Senior Full-stack Developer
- OpsFlow: Senior Software Engineer (.NET, Node.js, React)
- Karmah: Full Stack Developer
- CodeSignal: Software Engineer, Business Experience
- Cotiviti: SDET
- DexCom: Sr Web Applications Developer
- Vituity: Senior Web Applications Developer
- Edgewater Federal Solutions: .Net Junior-level Software Engineer
- Applied Imagination: BA/QA
- EBG: PHP Web Application Developer
- ImFusion: Backend / Web Application Developer
- Lumenalta: Javascript Fullstack Engineer - Senior
- Jiga: Full Stack Product Engineer
- Storetasker: Senior Shopify Developer
- WodBoard: Senior Rails Developer
- KFC: Sr. Software Engineer II
- comparis.ch: Experienced Full-Stack Software Engineer (.NET/React)
- Stack Influence: Senior Full Stack Javascript Engineer
- Shopify: Senior Software Engineer - Streaming Pipelines
- Shopify: Staff Software Engineer - Internal Tools
- Wrapbook: Senior Software Engineer II
- Wrapbook: Senior Software Engineer I
- Wrapbook: Software Engineer III
- AuditBoard: Senior Software Engineer II (Full Stack), BCM
- AuditBoard: Senior Software Engineer I, Localization
- AuditBoard: Staff Software Engineer, Architecture Team
- Fieldguide: Deployed Software Engineer
- Fieldguide: Software Engineer (All Levels)
- TENEX.AI: Senior Software Engineer
- TENEX.AI: Staff Software Engineer
- Cohere: Full-Stack Software Engineer, Inference

---

## 2026-01-22 (last_scrape timestamp + session history logging)

**Done:**
- Added `jobs/last_scrape` timestamp to `/job:scrape` - reads before fetching, skips JSON API entries older than last scrape, writes current time after successful run
- Date fields per source: `epoch`/`date` (RemoteOK), `publication_date` (Remotive), `pubDate` (Jobicy), `pub_date` (Working Nomads)
- HTML sources (WeWorkRemotely, Shopify, Ashby pages) skip date filtering, rely on dedup only
- Added session history logging step to all 3 job commands (`/job:source`, `/job:scrape`, `/job:analyze`)
- Added `jobs/last_scrape` to `.gitignore` (runtime state)
- Updated `JOB_PIPELINE_PLAN.md`: file tree, scrape steps, efficiency notes, files-to-create list, new career pages
- Updated `README.md`: structure, sources list, scrape pipeline description

**Suggested commit:**
```
Add last_scrape date filter and session history logging to job commands

Scrape command now reads jobs/last_scrape timestamp and skips JSON API
entries published before the last run. Writes new timestamp after each
successful scrape. All three job commands (/job:source, /job:scrape,
/job:analyze) now append their reports to .claude/session_history.md.
Updated plan and README to match.
```

---

## 2026-01-22 (source validation round 2 - add company career pages)

**Done:**
- Ran `/job:source` - validated all existing sources against freshness, effectiveness, and responsiveness
- All existing sources confirmed ACTIVE (RemoteOK, Remotive, Jobicy x5, Working Nomads, WeWorkRemotely, Shopify, Wrapbook, AuditBoard)
- LinkedIn/Indeed confirmed NEEDS_LOGIN (Playwright required)
- Discovered and added 3 new company career pages (all Ashby-hosted, WebFetch-compatible):
  - Fieldguide: 35 jobs, 8 engineering, remote USA (cybersecurity/audit)
  - TENEX.AI: 47 jobs, 27 engineering, remote USA (AI security)
  - Cohere: 70+ jobs, many engineering, remote/Canada (AI/ML, Toronto/Montreal)
- Evaluated but rejected: Arbeitnow (mostly German, only 1 remote), Himalayas (no category filtering, 100k+ unfiltered jobs), GitLab (0 current openings), Automattic/Zapier (no listings on careers page), Ramp (hybrid only), Suno (on-site), Nevoya (no dev roles), Arc.dev (no public API)
- No changes needed to scrape command (existing Ashby handler covers new sources)

**Suggested commit:**
```
Add Fieldguide, TENEX.AI, Cohere career pages to job sources

Validated all existing sources (all active). Added 3 new Ashby-hosted
career pages with remote engineering roles: Fieldguide (8 eng, remote
USA), TENEX.AI (27 eng, remote USA), Cohere (70+ roles, remote Canada).
All compatible with existing WebFetch Ashby handler in scrape command.
```

---

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
