---
description: Add new facts from recent work to the profile
argument-hint: <work-folder>
allowed-tools: Read, Edit, Grep, Glob, Bash(git:*), Bash(find:*), AskUserQuestion
---

Bring `jobs/profile/profile.txt` up to date from the work recorded in
$ARGUMENTS since the last refresh. Macha lives at `~/src/oceanhue/macha`,
where Claude Code is started; every Macha path below is relative to it.
Only facts the user approves are written, and every fact
rests on a source line that states it.

1. **Find the window.** The last refresh is the newest commit whose subject
   starts with "Refresh profile":
   ```bash
   git -C ~/src/oceanhue/macha log -1 --grep='^Refresh profile' --format=%cs \
     -- jobs/profile/profile.txt
   ```
   With no such commit, the window starts at the start date of the current
   employer named in the profile.

2. **Read the work.** In $ARGUMENTS, read every `.claude/session_history.md`
   entry dated inside the window, and the commit subjects authored by the
   user in each repository under it:
   ```bash
   git -C <repo> log --all --no-merges --author=Guigova --since=<date> \
     --format='%cs %s'
   ```
   Read only these sources. Skip files that hold credentials, recovery
   codes or personal documents.

3. **Draft facts.** For each significant piece of work — a system built, an
   integration, a production launch, a measured improvement, a new
   language or tool — draft one fact in the profile's voice. Each draft
   carries its source (`file:line` or repository and commit) and holds
   these rules:
   - A number is quoted from the source exactly as measured. A preview or
     staging figure is not presented as production, and a before-and-after
     pair compares the same measurement.
   - A mixed result is stated whole (7 of 7 on two sources and 4 of 7 on a
     third, not "7 of 7").
   - No customer names, account IDs, hostnames or colleague names.
   - A draft that changes an existing fact names the line it replaces.

4. **PAUSE — review.** Show each draft with its source and
   `AskUserQuestion` (multi-select): which to write. Corrections come
   through Other.

5. **Write and commit.** Write the approved facts, each replacing the line
   it supersedes or added beside the employer's other facts; a fact never
   appears twice. Commit `jobs/profile/profile.txt` alone with the subject
   `Refresh profile from <employer> work through <date>`.
