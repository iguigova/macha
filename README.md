# Macha

Job applications from one profile of facts, run through Claude Code.
`jobs/profile/` is the single source of truth: every resume, cover letter
and form answer is generated from it for the job at hand.

## Setup

```bash
claude mcp add --scope user --transport stdio playwright -- npx -y @playwright/mcp
cd ~/src/oceanhue/macha && claude
```

The skills live in `.claude/skills/` and load when Claude Code starts
in this directory. The steps `/job:apply` names, from picking a queued
posting to closing its note, live in `.claude/skills/job:apply/steps.md`.

The browser keeps its logins, so a site that needs one (LinkedIn, an ATS)
is logged into by hand once.

## Skills

```
/job:apply [<job-url> | <id>] # apply to one job; no argument takes the oldest queued one
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
Every change is committed. The notable repositories are not listed in the
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
