# macha-viewer

A lightweight Haskell web server for browsing job application markdown files.

## Build

```bash
cabal build
```

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

## Features

- Sidebar listing all applications, sorted alphabetically
- Color-coded fit badges (Strong / Good / Moderate / Stretch)
- Client-side search/filter by title or fit rating
- Markdown rendered to HTML with GFM support (tables, strikethrough, autolinks)
- Files read from disk on each request (new applications appear without restart)

## Test

```bash
cabal test
```

## Project Structure

```
macha-viewer/
  macha-viewer.cabal
  static/
    index.html            -- SPA frontend (HTML + CSS + JS)
  app/
    Main.hs               -- Entry point
  src/
    Viewer/
      Server.hs           -- Scotty routes
      Markdown.hs         -- File listing, metadata parsing, cmark rendering
      Types.hs            -- FileInfo type + JSON serialization
      Html.hs             -- Loads the static HTML shell
  test/
    Spec.hs               -- Test discovery
    Viewer/
      MarkdownSpec.hs     -- Unit tests for parsing and rendering
      ServerSpec.hs       -- Integration tests for HTTP endpoints
```

## Dependencies

- **scotty** -- HTTP routing
- **cmark-gfm** -- GitHub-flavored markdown rendering (C FFI)
- **aeson** -- JSON serialization
- **hspec** / **hspec-wai** -- Testing
