# Macha - Job Search Automation

Automated job search and application system powered by Claude Code. One command finds matching jobs, assesses fit, and applies — learning from corrections every time.

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

Given a count N (default 1), search for matching jobs using any available method:

- **WebSearch** — recent postings for "remote software engineer", "remote backend developer", etc.
- **LinkedIn** — via Playwright if logged in
- **Job board APIs** — RemoteOK (`remoteok.com/api`), Remotive (`remotive.com/api/remote-jobs`)
- **Career pages** — direct company job boards

For each candidate:
1. Read the job description
2. Quick fit assessment (2-3 sentences: what matches, what gaps)
3. Filter obvious mismatches: requires security clearance, on-site only, wrong country, completely unrelated stack
4. Check against `jobs/done/` filenames to avoid re-applying
5. Present results to user before proceeding

Bias toward applying — volume matters. If in doubt, keep it.

### Phase 2: Apply

For each job that passes fit:

1. Navigate to the job page via Playwright
2. Find and click the Apply button (Easy Apply modal, external ATS, whatever exists)
3. Generate a targeted cover letter — pick 3-4 career facts most relevant to this job
4. Fill the form from profile facts:
   - Identity: name, email, phone, location, LinkedIn, GitHub, website
   - Resume upload: `~/Downloads/IlkaGuigova+.pdf`
   - Cover letter (paste into textarea if available)
   - Screening questions (factual from profile, behavioral composed from career facts)
   - Demographics (gender, veteran status, disability, race from profile defaults)
5. Screenshot the completed form
6. **Pause for user review** — show summary of what's filled, flag uncertainties
7. User approves, provides corrections, or skips
8. On approve → submit → verify confirmation → save record to `jobs/done/`

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
- Cover letter used
- Screening answers given
- Any new facts learned during the application
