# Macha - Job Search Automation

Automated job search and application system powered by Claude Code. One command finds matching jobs and applies — learning from corrections every time.

## Setup

```bash
# Add Playwright MCP for browser automation (LinkedIn, ATS forms)
claude mcp add --transport stdio playwright -- npx -y @microsoft/playwright-mcp
```

On first use with LinkedIn, manually log in when the browser opens. The session persists for ~30 days.

## Usage

```bash
/job:apply 1                            # Find 1 matching job, apply
/job:apply 5                            # Find 5 matching jobs, apply to each
/job:apply https://linkedin.com/...     # Apply directly to this job
```

## How It Works

### Phase 1: Find

Given a count N (default 1), search using a tiered source ladder — stop as soon as N candidates are found:

1. **Tier 1 — Structured sources** (1 fetch = many jobs): HN Who is Hiring (`hnhiring.com`), Jobicy API, RemoteOK API, Ashby career pages
2. **Tier 2 — WebSearch** (only if Tier 1 < N): targeted `site:` queries against Ashby, Lever, Greenhouse
3. **Tier 3 — Playwright** (last resort): LinkedIn, WeWorkRemotely, Working Nomads

Each candidate passes through:
1. **Exclusion list** — check against `jobs/done/` to avoid re-applying
2. **Filter gate** — skip on-site, US-auth-required, wrong timezone, irrelevant titles (iOS/ML/PHP-only, etc.)
3. **Fit rubric** — 5-point score: language match, domain match, seniority, infra, location+comp. Include 2+, skip 0-1.
4. **Present results** with score breakdown before proceeding

### Phase 2: Apply

For each job that passes fit:

1. Navigate to the job page via Playwright
2. Find and click the Apply button (Easy Apply modal, external ATS, whatever exists)
3. **Cover letter (conditional)** — only if the form has a cover letter field. Pick 3-4 career facts most relevant to this job. If the form needs a file upload, generate a PDF via `soffice --headless --convert-to pdf`.
4. Fill the form from profile facts:
   - Identity: name, email, phone, location, LinkedIn, GitHub, website
   - Resume upload: `~/Downloads/IlkaGuigova+.pdf`
   - Cover letter (paste into textarea or upload PDF)
   - Screening questions (factual from profile, behavioral composed from career facts)
   - Demographics (gender, veteran status, disability, race from profile defaults)
5. **Pause for user review** — show summary of what's filled, flag uncertainties
6. User approves, provides corrections, or skips
7. On approve → submit → verify confirmation → save record to `jobs/done/`

### Learning Loop

User corrections become new facts in `jobs/profile/profile.txt`. The profile grows smarter with every application.

| Correction | What happens |
|---|---|
| "Say 15 years for C#, not 10" | Existing fact updated |
| "For 'willing to relocate' always say No" | New fact added |
| "On ClearCompany, skip the account step" | New fact added (application pattern) |
| "Shorten cover letters for small text fields" | New fact added (style guidance) |

Facts that contradict existing ones are updated in place — never duplicated.

## Structure

```
macha/
├── .claude/commands/
│   └── job:apply.md        # The single command
├── jobs/
│   ├── profile/
│   │   └── profile.txt     # Flat facts — the single source of truth
│   └── done/               # Completed application records
│       └── {company}_{role}.md
├── CLAUDE.md               # Project context for Claude Code
└── README.md               # This file
```

## Profile

`jobs/profile/profile.txt` is flat facts — no sections, no headers. Every piece of information is a natural language statement: identity, experience, career history, education, application defaults, cover letter style, and learned application patterns.

Everything (cover letters, screening answers, form data) is derived from these facts on demand.

### Updating the Profile

- Add new facts as natural language statements
- If a fact changes, update the existing statement (don't add a second version)
- If a fact contradicts another, remove the old one
- The `/job:apply` command adds facts automatically when you provide corrections

## Application Records

Successful applications are saved to `jobs/done/{company}_{role}.md` with:
- URL, company, role, timestamp
- Cover letter used (or "N/A" if no cover letter field)
- Form data with sources (which profile fact informed each field)
- Screening answers with sources
- Profile facts used
- New facts learned during the application
- User corrections applied
