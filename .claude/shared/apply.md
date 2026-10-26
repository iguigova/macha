# Applying: the shared steps

`/job:apply` and `/rfp:apply` read this file before their first step, and
each of their steps that names a section here runs it as written. Each
command names its kind, `job` or `rfp`, and its posting's name:
`<Company> — <Role>` for a job, `<Buyer> — <Title>` for an RFP. Every path
is relative to `~/src/oceanhue/macha`, where Claude Code is started.

## Evidence

Every claim in every document and answer comes from a fact in
`jobs/profile/profile.txt`, the GitHub site it names, or the credo the
global CLAUDE.md loads; nothing is invented, and a requirement no fact
covers is a gap, not a sentence.

## Pick the posting

Read `jobs/profile/profile.txt` in full, and the GitHub site it names. A
queued posting is a note, `<name>: <url>`, captured under a
`<kind>: <name>` topic; list them with
```bash
~/.claude/scripts/journal_queue.sh --topic "<kind>: "
```
The argument is a URL, or a queued note's id; with none, take the first
row, the oldest. A URL that a queued note carries is that posting. With no
argument and no queued posting, run the command's search, queue the
postings the user approves, and take the first. A URL that a closed note
carries, `journal_queue.sh --closed --topic "<kind>: "`, has been seen
before: name its note and where it went to the user and ask before going
on. A URL no note carries is queued first (Queue the approved postings), so
every posting is a note.

## Ask for what the profile lacks

Before the drafts are shown, and never folded into their approval: one
`AskUserQuestion` with one question per fact the posting weighs that no
fact covers, or that two facts state differently. Each question names the
requirement and offers concrete answers, each option saying what it would
write; a free answer comes through Other. Write each answer into the
profile at once (Update the profile) and redraft from it. A fact is never
written into a document before it is in the profile.

## Render the PDFs

Each document is an HTML file in `out/` linking
`../jobs/templates/style.css`, which holds the look; the HTML holds only
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
cd ~/src/oceanhue/macha/out
google-chrome --headless --no-pdf-header-footer \
  --print-to-pdf=$PWD/<Name>_<Doc>.pdf file://$PWD/<doc>.html
```
`pdfinfo` must report no more pages than the command's format allows; when
it reports more, `pdftotext -f <first page over>` shows what spilled. Fit
by balance: reword a line that leaves one word on the next, then adjust
spacing in the stylesheet; a fact the posting weighs is never dropped to
fit. Check the look with `pdftoppm -png` before going on: no line ends
with a lone word, and the space is even down to the footer. `out/` holds
only the current application and is overwritten each run.

## Update the profile

Each answer or correction that states a fact or a writing preference
updates the one line that covers it, or adds a line where none does: a
fact about the user in `profile.txt`, a rule for application writing under
the command's voice, and any other writing preference in
`~/.claude/credo.md`. A fact never appears twice, and one that contradicts
another line is asked about, never written beside it. A number is quoted
exactly as the user or the source states it, and a mixed result is stated
whole. A fact drawn from work names no customer, account ID, hostname or
colleague. Commit at once, after each round of answers or corrections, the
files changed alone with a message naming each fact changed:
```bash
git -C ~/src/oceanhue/macha commit -m "<message>" -- jobs/profile/profile.txt .claude/commands/<kind>:apply.md
~/.claude/scripts/tooling_commit.sh -m "<message>" -- ~/.claude/credo.md
```

## Close the note

Every run ends with one journal entry under the posting's topic that
closes its note, its reason what happened: applied, the posting found
closed, or skipped and why.
```bash
~/.claude/scripts/journal.sh --via /<kind>:apply -t "<kind>: <name>" \
  -b "<what happened, and what the page showed>" \
  --placed "<id>: - — <what happened>"
```

## Check the profile's integrity

By the principles in the credo. Read `profile.txt` whole and list, with
line numbers and both lines quoted:
- two lines that contradict: a span of years against an employer's dates,
  a claim against a "no experience" line, one fact stated two ways;
- a fact stated twice;
- a fact copied outside `profile.txt`, found by grepping its value in
  `CLAUDE.md`, `README.md` and `.claude/`.

Ask about each contradiction, one question each: which line is wrong. The
wrong line is deleted, or corrected only to what the user states; the
check that found it never supplies the replacement. A duplicate is
removed, and a copy becomes a pointer to its file. Commit as in Update the
profile. With nothing found, say what was checked. Then say it is a safe
point to `/compact`: the committed profile and the journal carry what the
next application needs.

## Searching

A search fills an empty queue, from the command's sources and by its fit
rules. Nothing outside the profile narrows it.

1. **Read the postings already seen**, open and closed; each note carries
   its URL:
   ```bash
   ~/.claude/scripts/journal_queue.sh --topic "<kind>: "
   ~/.claude/scripts/journal_queue.sh --closed --topic "<kind>: "
   ```
   Every candidate is held against them.

2. **Search every source** the command lists, and note a source that
   failed rather than dropping it silently.

3. **Check each candidate at its source.** A listing is a lead; the
   posting is the fact. Follow each lead to the posting itself, confirm it
   is open, and read it for what the command's fit rules weigh. Keep it
   only if it is open and passes them. A posting whose URL a note carries
   is dropped. A posting from a company or buyer a note names is kept and
   marked with that note, so the user decides.

4. **PAUSE — the shortlist.** Rank by fit, closest first, by the command's
   ranking. Show one table: rank, the posting's name, the columns the
   command names, the profile facts it fits, the note it shares a company
   or buyer with if any, and the URL. Under it, list each source and how
   many candidates it gave, and each source that failed. Then
   `AskUserQuestion`: "Queue which of these?" with options "All" and
   "None"; a choice of ranks comes through Other. On None, the run stops
   with nothing queued.

5. **Queue the approved postings**, one journal call each, best fit first,
   so the best fit is the oldest note and the first one taken:
   ```bash
   ~/.claude/scripts/journal.sh --via /<kind>:apply -t "<kind>: <name>" \
     -b "<url> — <what the command's fit rules found>; fits <the facts it fits>" \
     -n "<name>: <url>"
   ```
   Then show the queue with `journal_queue.sh --topic "<kind>: " --show`
   and take its first row.
