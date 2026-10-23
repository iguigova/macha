# Macha

Job applications from one profile of facts, run through Claude Code.
`jobs/profile/profile.txt` is the single source of truth: every resume,
cover letter and form answer is generated from it for the job at hand.

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
/job:batch [what to look for] # find open jobs that fit and queue the approved ones
/job:apply [<job-url> | <id>] # apply to one job; no argument takes the oldest queued one
/job:refresh <work-folder>    # add facts from recent work to the profile
```

`/job:batch` searches the ATS boards, HN Who's Hiring, the remote job boards
and LinkedIn, confirms each posting is open at its source, and drops the ones
the job log already holds. It shows a ranked shortlist and queues the jobs
the user approves.

`/job:apply` reads the job, drafts a gap analysis, a one-page resume and a
cover letter, and pauses for approval. It then renders the PDFs into `out/`,
fills the form, and pauses again before submitting. The documents take
their look from `jobs/templates/style.css`. Every run ends with a journal
entry recording the outcome: applied, closed or skipped.

`/job:refresh` reads the work recorded in a folder since the last refresh —
session histories and the user's commits — drafts facts with their sources,
and writes only the approved ones.

## The profile

`profile.txt` is flat facts, one per line: identity, experience, career
history, education, application answers, and the writing voice. A fact
appears once: a change updates its line, never adds a second version.
Corrections made during `/job:apply` and facts approved in `/job:refresh`
are the two ways it changes, and each is committed.

## The job log

The session history, `.claude/session_history.md`, is the log of every job.
Each job's entries share one topic, `job: <Company> — <Role>`, and each one's
first bullet is the outcome and the link: `queued`, `applied`, `closed` or
`skipped`, then the URL. `.claude/topic_kinds` lists `job`, so the journal
writer takes that topic without a ticket key. `.claude/scripts/job_log.sh`
prints every job at its latest outcome.

A queued job is a note in the journal's queue, `<Company> — <Role>: <url>`,
which `/job:batch` opens. `/job:apply` with no argument takes the oldest one and closes it when the
job is applied to, found closed or skipped. The queue lists them:

```bash
~/.claude/scripts/journal_queue.sh --topic "job: " --show
```
