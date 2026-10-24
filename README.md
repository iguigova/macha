# Macha

Job applications from one profile of facts, run through Claude Code.
`jobs/profile/` is the single source of truth: every resume, cover letter
and form answer is generated from it for the job at hand.

## Setup

```bash
claude mcp add --scope user --transport stdio playwright -- npx -y @playwright/mcp
cd ~/src/oceanhue/macha && claude
```

The commands live in `.claude/commands/` and load when Claude Code starts
in this directory.

The browser keeps its logins, so a site that needs one (LinkedIn, an ATS)
is logged into by hand once.

## Commands

```
/job:apply [<job-url> | <id>] # apply to one job; no argument takes the oldest queued one
```

With no argument and an empty queue, `/job:apply` first searches company
boards, HN Who's Hiring, the remote job boards and LinkedIn. It keeps a
posting for the kind of work the profile looks for, in any stack; the
preferred stack only ranks it. It confirms each posting is open at its
source and drops the ones the job log already holds. It shows a ranked
shortlist, queues the jobs the user approves, and takes the first.
`.claude/scripts/job_board.sh` reads a Greenhouse, Ashby or Lever board, or
checks that one of its postings is open.

For each job, `/job:apply` reads the job, drafts a gap analysis, a one-page resume and a
cover letter, and pauses for approval. It then renders the PDFs into `out/`,
fills the form, and pauses again before submitting. The documents take
their look from `jobs/templates/style.css`. Every run ends with a journal
entry recording the outcome (applied, closed or skipped), then checks the
profile for contradictions, duplicates and copies by the principles in
the credo.

## The profile

The profile is flat facts, one per line, in `jobs/profile/profile.txt`:
identity, experience, career history, education and application answers.
Who the user is and how they work lives in `~/.claude/credo.md`, which the
global CLAUDE.md loads in every project. The application voice and formats
live in `/job:apply`. A fact appears once: a change updates its line,
never adds a second version. `/job:apply` asks for each fact a job weighs
that the profile lacks, and writes the answers and corrections at once.
Every change is committed.

## The job log

The session history, `.claude/session_history.md`, is the log of every job.
Each job's entries share one topic, `job: <Company> — <Role>`, and each one's
first bullet is the outcome and the link: `queued`, `applied`, `closed` or
`skipped`, then the URL. `.claude/topic_kinds` lists `job`, so the journal
writer takes that topic without a ticket key. `.claude/scripts/job_log.sh`
prints every job at its latest outcome.

A queued job is a note in the journal's queue, `<Company> — <Role>: <url>`,
which the search opens. `/job:apply` with no argument takes the oldest one
and closes it when the job is applied to, found closed or skipped. The queue lists them:

```bash
~/.claude/scripts/journal_queue.sh --topic "job: " --show
```
