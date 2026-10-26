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
in this directory. The steps applying shares across kinds, from picking a
queued posting to closing its note, live once in `.claude/shared/apply.md`,
which each apply command reads first.

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
`.claude/scripts/rfp_board.sh` does the same for public procurement: the
open notices of CanadaBuys, the BC Digital Marketplace, one Bonfire portal or
CivicInfo BC, or whether one notice is still open.

For each job, `/job:apply` reads the job, drafts a gap analysis, a one-page resume and a
cover letter, and pauses for approval. It then renders the PDFs into `out/`,
fills the form, and pauses again before submitting. The documents take
their look from `jobs/templates/style.css`. Every run ends with a journal
entry closing the job's note with what happened, then checks the
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
Every change is committed. The notable repositories are not listed in the
profile: it names the GitHub site, https://iguigova.github.io, which
lists them, and `/job:apply` reads it there.

## The job log

Every job and every RFP is a note in the journal's queue, written by
`journal.sh -n` into `.claude/session_history.md`. A job's note is
`<Company> — <Role>: <url>`, captured under the topic
`job: <Company> — <Role>`; an RFP's is `<Buyer> — <Title>: <url>`, under
`rfp: <Buyer> — <Title>`. `.claude/topic_kinds` lists `job` and `rfp`, so
the journal writer takes those topics without a ticket key.

A note is open while the job or RFP waits, and closed by `--placed` with
what happened: applied, the posting found closed, or skipped and why.
`/job:apply` with no argument takes the oldest open job note and closes it;
no command applies to an RFP yet. The queue lists each kind's open notes,
and with `--closed` its closed ones; `rfp: ` lists the RFPs the same way:

```bash
~/.claude/scripts/journal_queue.sh --topic "job: " --show
~/.claude/scripts/journal_queue.sh --closed --topic "job: " --show
```

A freelance or contract posting is a job, and so is a public-sector
contract working through a firm on a federal supply list (TBIPS): each is
found and applied to as one. A contract the user wins and finishes is
recorded in the profile, since an RFP scores the bidder's own past
contracts.
