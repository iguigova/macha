# macha-viewer

A lightweight Haskell web server for browsing and triaging job application markdown files. Part of the [Macha](../README.md) job search automation system.

## Build

```bash
cabal build
```

Requires GHC >= 9.4 and Cabal >= 3.8.

## Run

```bash
cabal run macha-viewer -- [PORT] [DIRECTORY]
```

Defaults: port `3000`, directory `jobs/applications`.

Example:

```bash
cabal run macha-viewer -- 3000 ../jobs/applications
```

Then open http://localhost:3000.

The server expects the following directory layout (sibling directories relative to the one you pass):

```
jobs/
  applications/     -- New applications to review (default view)
  queue/            -- Applications queued for later
  done/             -- Applications marked as applied
  discarded/        -- Applications you've passed on
```

## Features

### Browsing

- **Directory tabs**: Switch between applications, queue, done, and discarded directories
- **Resizable sidebar**: Drag the handle between the sidebar and content area (180px-500px)
- **Client-side search**: Filter files by title or fit rating in real time
- **Sort controls**: Sort the file list by fit rating, modification date, or name
- **Color-coded fit badges**: Visual indicators for Strong (green), Good (blue), Moderate (orange), and Stretch (red)
- **Markdown rendering**: Full GitHub-Flavored Markdown support including tables, strikethrough, and autolinks
- **Live file listing**: Files are read from disk on each request; new applications appear without restart

### Triage Actions

- **Apply**: Moves the current file from its directory to `jobs/done/`
- **Discard**: Moves the current file from its directory to `jobs/discarded/`
- Actions are available when viewing `applications` or `queue` directories

### Notes

- Each file has a notes textarea below the rendered content
- Notes are saved automatically (debounced 800ms) as a `## Notes` section appended to the markdown file
- Existing notes are loaded when viewing a file

### Stats Footer

- Displays file counts for all directories: applications, done, discarded, queue
- Shows daily, weekly, and monthly breakdowns based on file modification times
- Updates automatically after apply/discard actions

## Expected Markdown Format

Application files should follow this structure:

```markdown
# Company Name - Job Title

**URL:** https://example.com/jobs/123
**Fit:** Strong fit

## Job Description

...

## Fit Assessment

**Alignment:**
- Relevant experience point

**Gaps:**
- Missing requirement

## Cover Letter

Dear Hiring Team,
...
```

The viewer extracts:
- **Title** from the first `# ` heading (falls back to filename)
- **URL** from the `**URL:**` field
- **Fit** from the `**Fit:**` field (used for badge color and sort order)
- **Timestamp** from the file's modification time (used for date sort)

## API

All file endpoints accept a `:dir` parameter (`applications`, `queue`, `done`, `discarded`).

| Method | Route | Description |
|--------|-------|-------------|
| GET | `/` | Serves the SPA frontend |
| GET | `/api/:dir/files` | List all `.md` files with parsed metadata |
| GET | `/api/:dir/file/:name` | Render a markdown file to HTML |
| POST | `/api/:dir/file/:name/apply` | Move file to `done/` |
| POST | `/api/:dir/file/:name/discard` | Move file to `discarded/` |
| POST | `/api/:dir/file/:name/notes` | Update the `## Notes` section (body = plain text) |
| GET | `/api/stats` | File counts per directory with time breakdowns |

### Example responses

`GET /api/applications/files`:
```json
[
  {
    "filename": "acme-corp-swe.md",
    "title": "Acme Corp - Software Engineer",
    "url": "https://example.com/jobs/123",
    "fit": "Strong fit",
    "timestamp": 1706000000
  }
]
```

`GET /api/stats`:
```json
{
  "applications": { "total": 27, "today": 3, "week": 12, "month": 27 },
  "done": { "total": 5, "today": 1, "week": 3, "month": 5 },
  "discarded": { "total": 2, "today": 0, "week": 1, "month": 2 },
  "queue": { "total": 15, "today": 5, "week": 10, "month": 15 }
}
```

## Test

```bash
cabal test
```

38 tests covering:
- Metadata parsing (title, URL, fit extraction, edge cases)
- File listing and filtering
- GFM markdown rendering (tables, strikethrough, autolinks)
- File move operations (apply/discard)
- Notes update and replacement
- Directory stats computation
- All HTTP endpoints (GET/POST with directory parameterization)

## Project Structure

```
macha-viewer/
  macha-viewer.cabal
  static/
    index.html              -- SPA frontend (HTML + CSS + JS, single file)
  app/
    Main.hs                 -- Entry point (arg parsing, server startup)
  src/
    Viewer/
      Types.hs              -- FileInfo, DirStats types + JSON serialization
      Markdown.hs           -- File listing, metadata parsing, move, notes, stats
      Server.hs             -- Scotty routes + request handling
      Html.hs               -- Loads the static HTML shell from disk
  test/
    Spec.hs                 -- hspec-discover test runner
    Viewer/
      MarkdownSpec.hs       -- 28 unit tests for parsing, move, notes, stats
      ServerSpec.hs         -- 10 integration tests for HTTP endpoints
```

## Dependencies

| Package | Purpose |
|---------|---------|
| scotty | HTTP routing and request handling |
| cmark-gfm | GitHub-Flavored Markdown to HTML (C FFI bindings) |
| aeson | JSON serialization for API responses |
| wai / wai-extra | WAI middleware (request logging) |
| directory | File system operations (list, move, mtime) |
| time | Timestamp handling for stats computation |
| hspec | Test framework |
| wai-extra (Network.Wai.Test) | HTTP integration testing |
