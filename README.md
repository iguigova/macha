# Macha

Job applications from one profile of facts, run through Claude Code.
`jobs/profile/` is the single source of truth: every resume, cover letter
and form answer is generated from it for the job at hand.

## Setup

```bash
claude mcp add --scope user --transport stdio playwright -- npx -y @playwright/mcp \
  --isolated --storage-state ~/.config/playwright-mcp/state.json
cd ~/src/oceanhue/macha && claude
```

The skills live in `.claude/skills/` and load when Claude Code starts
in this directory. The steps `/job:apply` names, from picking a queued
posting to closing its note, live in `.claude/skills/job:apply/steps.md`.

Each session's browser starts from the logins saved in the file that
`~/.claude.json` passes the playwright server as `--storage-state`. A site
that needs one (LinkedIn, an ATS) is logged into with
`~/.claude/scripts/playwright_login.sh <login-url>`, which saves the login
for every later session.

## Skills

```
/job:apply [<job-url> | <id>] # apply to one job; no argument takes the oldest queued one
/linkedin:digest              # mail the user the gist of their LinkedIn feed
```

With no argument and an empty queue, `/job:apply` first searches company
boards, HN Who's Hiring, the remote job boards and LinkedIn. It keeps a
posting for the kind of work the profile looks for, in any stack. Niche
postings, a small company with one specific, unusual problem, rank first;
the preferred stack only ranks after that. It confirms each posting is open at its
source and drops the ones the job log already holds. It shows a ranked
shortlist, queues the jobs the user approves, and takes the first.
`.claude/scripts/job_board.sh` reads a Greenhouse, Ashby or Lever board, or
checks that one of its postings is open.

For each job, `/job:apply` reads the job, drafts a gap analysis, a one-page resume and a
cover letter, and pauses for approval. It then renders the PDFs into `out/`,
fills the form, and pauses again before submitting. The documents take
their look from `jobs/templates/style.css`. Every run ends with a journal
entry closing the job's note with what happened, then checks the
profile for contradictions, duplicates, copies, lines holding more than
one fact and qualifiers that read as a limit, by the principles in the
credo. `.claude/scripts/profile_lint.sh` lists the lines that join two
sentences or two clauses, or carry a redundant "professional"; a list of
facts joined by commas is found by reading.

## The profile

The profile is a set of single facts, one per line, in
`jobs/profile/profile.txt`: identity, experience, career history, education
and application answers. How a line's qualifiers are read is stated in
`.claude/skills/job:apply/steps.md` (Evidence).
Who the user is and how they work lives in `~/.claude/credo.md`, which the
global CLAUDE.md loads in every project. The application voice and formats
live in `/job:apply`. A fact appears once: a change updates its line,
never adds a second version. `/job:apply` asks for each fact a job weighs
that the profile lacks, and writes the answers and corrections at once.
Every change is committed through `~/.claude/scripts/tooling_commit.sh`,
gated by `.claude/scripts/selftest.sh`, and a changed skill description by
its prompts in `.claude/skill_triggers.json`. The notable repositories are not listed in the
profile: it names the GitHub site, https://iguigova.github.io, which
lists them, and `/job:apply` reads it there.

## The job log

Every job is a note in the journal's queue, written by
`journal.sh -n` into `.claude/session_history.md`, which git ignores, so
the log stays on this machine. A job's note is
`<Company> — <Role>: <url>`, captured under the topic
`job: <Company> — <Role>`. `.claude/topic_kinds` lists `job`, so the
journal writer takes that topic without a ticket key.

A note is open while the job waits, and closed by `--placed` with
what happened: applied, the posting found closed, or skipped and why.
`/job:apply` with no argument takes the oldest open job note and closes it.
The queue lists the open job notes, and with `--closed` the closed ones:

```bash
~/.claude/scripts/journal_queue.sh --topic "job: " --show
~/.claude/scripts/journal_queue.sh --closed --topic "job: " --show
```

A freelance or contract posting is a job: it is found and applied to as one.

## The LinkedIn digest

`/linkedin:digest` reads the LinkedIn feed in the saved login, distils it
into a one-page gist PDF by the `gist` skill, and mails it with
`.claude/scripts/gist_mail.py`: the subject is the gist's title, the body
its text, the PDF attached, from and to the address the profile's email
line states. The mail goes through smtp.gmail.com:587 with a Gmail app
password in `.secrets/gmail-app-password`, mode 600; git ignores
`.secrets/`. Each day's feed, gist and PDF stay in
`.claude/linkedin_digest/`, also ignored.

`.claude/scripts/linkedin_digest.sh install` sets a systemd user timer
that runs the skill at 07:00 each day, headless, and catches up on a
missed morning. A run that sends nothing opens a journal note naming why;
`.claude/linkedin_digest.log` holds each run, and
`linkedin_digest.sh status` shows the timer and the last send.
