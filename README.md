# Macha

Job applications from one profile of facts, run through Claude Code.
`jobs/profile/profile.txt` is the single source of truth: every resume,
cover letter and form answer is generated from it for the job at hand.

## Setup

```bash
claude mcp add --scope user --transport stdio playwright -- npx -y @playwright/mcp
ln -s ~/src/oceanhue/macha/commands/job:apply.md ~/.claude/commands/
ln -s ~/src/oceanhue/macha/commands/job:refresh.md ~/.claude/commands/
```

The commands live in `commands/` and are linked into `~/.claude/commands/`,
so they run from any directory.

The browser keeps its logins, so a site that needs one (LinkedIn, an ATS)
is logged into by hand once.

## Commands

```
/job:apply <job-url>          # apply to one job
/job:refresh <work-folder>    # add facts from recent work to the profile
```

`/job:apply` reads the job, drafts a gap analysis, a one-page resume and a
cover letter, and pauses for approval. It then renders the PDFs into `out/`,
fills the form, and pauses again before submitting. The documents take
their look from `jobs/templates/style.css`.

`/job:refresh` reads the work recorded in a folder since the last refresh —
session histories and the user's commits — drafts facts with their sources,
and writes only the approved ones.

## The profile

`profile.txt` is flat facts, one per line: identity, experience, career
history, education, application answers, and the writing voice. A fact
appears once: a change updates its line, never adds a second version.
Corrections made during `/job:apply` and facts approved in `/job:refresh`
are the two ways it changes, and each is committed.
