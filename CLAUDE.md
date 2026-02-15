# Macha - Job Search Automation

## Project Purpose
Automated job search and application system. One command: find matching jobs, apply with human-in-the-loop.

## How It Works

`/job:apply N` finds N matching jobs from any source and applies to each — pausing before every submit for user review. Corrections become new facts in the profile, so each run gets smarter.

All data lives in `jobs/profile/profile.txt` — flat facts, no sections. Everything (cover letters, screening answers, form data) is derived from these facts on demand.

## User Profile (Quick Reference)

**Ilka Guigova** - Senior Software Engineer, 20+ years
- Location: Brackendale, BC, Canada (Remote only)
- Email: iguigova@gmail.com
- GitHub: github.com/iguigova
- LinkedIn: linkedin.com/in/iguigova
- CV: https://urgh.weebly.com/curriculum-vitae.html
- website: https://urgh.weebly.com/

**Primary Languages:** Go, C#, Java, Kotlin, TypeScript

**Strongest Areas:**
- REST API design (~15 years)
- Auth/authz (OAuth, OIDC, SAML) (~15 years)
- Relational databases (~10+ years)
- 0→1 systems building

## Working Style

- After completing a set of changes, include a suggested commit message at the end of the report and log it to `.claude/session_history.md`.
