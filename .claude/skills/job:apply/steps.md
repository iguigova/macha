# Applying: the named steps

Each step of `/job:apply` that names a section here runs it as written.
Every path is relative to `~/src/oceanhue/macha`, where Claude Code is
started.

## Evidence

Every claim in every document and answer comes from a fact in
`jobs/profile/profile.txt`, the GitHub site it names, or the credo the
global CLAUDE.md loads; nothing is invented, and a requirement no fact
covers is a gap, not a sentence. Experience in the profile is paid work
unless its line says otherwise, such as "from personal projects".

## Pick the posting

Read `jobs/profile/profile.txt` in full, and the GitHub site it names. A
queued posting is a note, `<Company> — <Role>: <url>`, captured under a
`job: <Company> — <Role>` topic; list them with
```bash
~/.claude/scripts/journal_queue.sh --topic "job: "
```
The argument is a URL, or a queued note's id; with none, take the first
row, the oldest. An argument `reply <id> <what happened>` is a reply to
an application: Record a reply, and stop. A URL that a queued note carries is that posting. With no
argument and no queued posting, run the search in `/job:apply`'s
Finding jobs, which queues the postings the user approves and takes the
first. A URL that a closed note carries,
`journal_queue.sh --closed --topic "job: "`, has been seen before:
name its note and where it went to the user and ask before going on. A
URL no note carries is queued first, as Finding jobs queues an approved
posting, so every posting is a note.

## Ask for what the profile lacks

Before the drafts are shown, and never folded into their approval: one
`AskUserQuestion` with one question per fact the posting weighs that no
fact covers, or that two facts state differently. Each question names the
requirement and offers concrete answers, each option saying what it would
write; a free answer comes through Other. Write each answer that is a
profile line (Update the profile) at once and redraft from it; an answer
that the user lacks it writes nothing, and the requirement stays a gap of
this application. A fact is never written into a document before it is in
the profile.

## Render the PDFs

Each document is an HTML file in `out/` linking
`../jobs/templates/style.css`, which holds the look, its colours and fonts
imported from brand.css; the HTML holds only
content in this markup:
- Every document: `<header>` with `<h1>` name and `.contact`; `<main>`;
  `<footer>` with `.sites` (the three themed websites the profile names)
  and an empty `.band`.
- Resume: under the name a `.tagline`; one `<section>` per resume section,
  each an `<h2>` label and one content block; Skills as a `<dl>`; each
  employer an `.entry` with a `.row` of `.org` and `.when` and a `<ul>`. A
  second line of detail under an entry is a `<br>` and a `.role` span.
- Letter: `<body class="letter">`, a `.meta` block with the date and the
  posting's name, the paragraphs, and `.sign`.

Print each, named for the user: `<Name>` is the profile's name with its
spaces removed, and `<Doc>` the document, such as `Resume`.
```bash
test -r ~/src/oceanhue/brand.css || { echo "no ~/src/oceanhue/brand.css: the PDFs would print unstyled" >&2; exit 1; }
cd ~/src/oceanhue/macha/out
google-chrome --headless --no-pdf-header-footer \
  --print-to-pdf=$PWD/<Name>_<Doc>.pdf file://$PWD/<doc>.html
```
`pdfinfo` must report no more pages than Application formats allows; when
it reports more, `pdftotext -f <first page over>` shows what spilled. Fit
by balance: reword a line that leaves one word on the next, then adjust
spacing in the stylesheet; a fact the posting weighs is never dropped to
fit. Check the look with `pdftoppm -png` before going on: no line ends
with a lone word, and the space is even down to the footer. `out/` holds
only the current application and is overwritten each run.

## Update the profile

Each answer or correction that states a fact or a writing preference
updates the one line that covers it, or adds a line where none does, one
fact to a line. A profile line states what the user has done, knows or
is, and is one an application could use; a lack, or a remark that says
nothing about the work, is never written. Each goes where it belongs: a
fact about the user in `profile.txt`, a rule for
application writing under Application voice in `/job:apply`, and any
other writing preference in `~/.claude/credo.md`. A fact never appears
twice, and one that contradicts another line is asked about, never
written beside it. A place, a date or a qualifier such as "professional"
or "personally" is written only where it is the whole truth, since a
line that has one reads as a limit on the lines that lack it. A number
is quoted exactly as the user or the source states it, and a mixed
result is stated whole. A fact drawn from work names no customer that is
not public, and no account ID, hostname or colleague. Commit at once,
after each round of answers or corrections, the files changed alone with
a message naming each fact changed:
```bash
~/.claude/scripts/tooling_commit.sh -m "fix(profile): <message>" -- jobs/profile/profile.txt .claude/skills/job:apply/SKILL.md
~/.claude/scripts/tooling_commit.sh -m "<message>" -- ~/.claude/credo.md
```

## Close the note

Every run ends with one journal entry under the posting's topic that
closes its note, its reason what happened: applied, the posting found
closed, or skipped and why.
```bash
~/.claude/scripts/journal.sh --via /job:apply -t "job: <Company> — <Role>" \
  -b "<what happened, and what the page showed>" \
  --placed "<id>: - — <what happened>"
```

## Record a reply

A reply is one journal entry under the topic of the note the id names,
open or closed (`journal_queue.sh --closed --topic "job: "`), its first
bullet `Reply: <kind> — <what happened>`, the kind one word: interview,
rejection, offer or silence.
```bash
~/.claude/scripts/journal.sh --via /job:apply -t "job: <Company> — <Role>" \
  -b "Reply: <kind> — <what happened>"
```
`.claude/scripts/job_log.py` then counts it under the source the job was
found on.

## Check the profile's integrity

By the principles in the credo. Run `.claude/scripts/profile_lint.sh`
and judge each row it prints, then read `profile.txt` whole and list, with
line numbers and both lines quoted:
- two lines that contradict: a span of years against an employer's dates,
  a claim against a "no experience" line, one fact stated two ways;
- a fact stated twice;
- a line holding more than one fact, including facts joined into a list
  by commas; a list of like things, such as interests, is one fact;
- a place, a date or a qualifier that reads as a limit (Update the
  profile);
- a lack, or a line no application could use (Update the profile);
- a writing rule, which belongs where Update the profile puts it;
- a fact copied outside `profile.txt`, found by grepping its value in
  `CLAUDE.md`, `README.md` and `.claude/`.

Ask about each contradiction, one question each: which line is wrong. The
wrong line is deleted, or corrected only to what the user states; the
check that found it never supplies the replacement. A line holding more
than one fact is split, its wording kept. Ask about each qualifier, one
question each: whether it is the whole truth; it is dropped or kept as
the user states. A duplicate is removed, a writing rule moved, and a copy
becomes a pointer to its file. Commit as in Update the profile. With
nothing found, say what was checked. Then say it is a safe
point to `/compact`: the committed profile and the journal carry what the
next application needs.
