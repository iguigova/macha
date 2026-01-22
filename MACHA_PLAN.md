# Macha

## Overview
A layered application for job matching using a **fact-based data model**. Everything is a fact. Profiles are root facts. Applications pair facts together. Tags describe facts.

## Core Concept: Everything is a Fact

```
Root Facts (owner_id = NULL)
    │
    ├── User profile ──┬── skills, experience, contact info
    │                  │
    ├── Job posting ───┼── requirements, company info
    │                  │
    └── Application ───┴── cover letter, match score, status
                       │
               application table
               (graph edges linking facts)
```

**Key insight**: Everything is a fact. Users, jobs, and applications are all root facts. They own child facts. The `application` table links facts together in a graph.

## Technology Stack

- **Language**: Haskell (GHC2021)
- **Build**: Cabal
- **Database**: SQLite with FTS5 (full-text search)
- **Core Library**: Pure business logic, no I/O assumptions
- **CLI Client**: optparse-applicative
- **API Client**: Servant (REST/JSON over HTTP)
- **Scraper**: Playwright MCP (authenticated LinkedIn access)
- **LLM**: Anthropic Claude API

## Architecture: Layered Design

```
┌─────────────────────────────────────────────────────┐
│  Clients (interchangeable, separate packages)       │
│  ┌───────────┐  ┌───────────┐  ┌─────────────────┐  │
│  │    CLI    │  │    API    │  │    Scraper      │  │
│  │ (optparse)│  │ (servant) │  │  (playwright)   │  │
│  │ macha-cli │  │ macha-api │  │ macha-scraper   │  │
│  └─────┬─────┘  └─────┬─────┘  └───────┬─────────┘  │
└────────┼──────────────┼────────────────┼────────────┘
         │              │                │
         ▼              ▼                ▼
┌─────────────────────────────────────────────────────┐
│  Macha.Core (pure library)                          │
│  - Types (Tag, Fact, Application)                   │
│  - Business Logic (matching, pairing)               │
│  - Database Operations (SQLite)                     │
│  - LLM Integration (extraction, generation)         │
│  - NO terminal I/O, NO HTTP assumptions             │
└─────────────────────────────────────────────────────┘
```

**Benefits:**
- Core library testable in isolation
- CLI and API share same logic
- Scraper runs independently, feeds facts to core
- Publishable as network service

## Data Model

### Design Principles

1. **Everything is a Fact**: Profiles, jobs, applications, artifacts - all facts
2. **Facts own Facts**: Hierarchical ownership via owner_id (null = root)
3. **Tags describe Facts**: Flexible categorization via n:n join
4. **Application table pairs Facts**: Graph edges connecting facts together

### Schema Diagram

```
USER (root fact)                    JOB (root fact)
    │                                   │
    ├── skill: OAuth                    ├── req: OAuth 2.0
    ├── skill: Go                       ├── req: GoLang
    └── exp: Bread&Butter               └── req: distributed
         │                                   │
         │      APPLICATION (root fact)      │
         │           │                       │
         │           ├── cover-letter        │
         │           ├── match-score         │
         │           └── status              │
         │                                   │
         └───────────┬───────────────────────┘
                     │
             application table
             (links facts together)
```

### SQL Schema

```sql
-- Tags describe facts
CREATE TABLE tag (
    id TEXT PRIMARY KEY,
    description TEXT,
    status TEXT,
    created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- Facts are everything (root facts have owner_id = NULL)
CREATE TABLE fact (
    id TEXT PRIMARY KEY,
    description TEXT,
    source TEXT,
    status TEXT,
    owner_id TEXT REFERENCES fact(id),
    created_at TEXT NOT NULL DEFAULT (datetime('now'))
);

-- FactTag: n:n between Fact and Tag
CREATE TABLE fact_tag (
    fact_id TEXT NOT NULL REFERENCES fact(id),
    tag_id TEXT NOT NULL REFERENCES tag(id),
    created_at TEXT NOT NULL DEFAULT (datetime('now')),
    PRIMARY KEY (fact_id, tag_id)
);

-- Application: pairs two facts (graph edges)
CREATE TABLE application (
    fact_id1 TEXT NOT NULL REFERENCES fact(id),
    fact_id2 TEXT NOT NULL REFERENCES fact(id),
    created_at TEXT NOT NULL DEFAULT (datetime('now')),
    PRIMARY KEY (fact_id1, fact_id2)
);

-- Indexes
CREATE INDEX idx_fact_owner ON fact(owner_id);
CREATE INDEX idx_fact_tag_fact ON fact_tag(fact_id);
CREATE INDEX idx_fact_tag_tag ON fact_tag(tag_id);
CREATE INDEX idx_app_fact1 ON application(fact_id1);
CREATE INDEX idx_app_fact2 ON application(fact_id2);
```

### How Application Works

The `application` table links facts together:
- `(app-root, user-root)` → application involves this user profile
- `(app-root, job-root)` → application involves this job
- `(user-skill, job-requirement)` → specific fact match

Application root fact owns child facts:
- cover letter, match analysis, status updates, Q&A answers

### Examples

**Root facts (owner_id = NULL):**
- User profile (tagged: user)
- Job posting (tagged: job)
- Application (tagged: application)

**Child facts:**
- Skills, experience → owned by user profile
- Requirements → owned by job
- Cover letter, match score → owned by application

## CLI Commands

```bash
macha init                            # Initialize database

# Facts (everything is a fact)
macha fact create "description"       # Create root fact (profile)
macha fact create "skill" --owner <id> --tag skill
macha fact list                       # List root facts
macha fact list --owner <id>          # List facts under owner
macha fact show <id>                  # Show fact + children
macha fact import <file-or-url>       # Import from source (LLM extraction)

# Tags
macha tag create "user"               # Create tag
macha tag list
macha tag facts <tag>                 # List facts with tag

# Applications (pair facts)
macha apply <fact1> <fact2>           # Pair two facts
macha applications                    # List all pairings
macha applications --fact <id>        # Pairings involving fact

# LLM operations
macha extract <source>                # Extract facts from source
macha match <fact1> <fact2>           # Evaluate match between facts
macha generate cover-letter <user-fact> <job-fact>
```

## Project Structure

```
macha/
├── .gitignore
├── cabal.project            # Workspace configuration
├── src/
│   ├── macha-core/          # Pure library (no I/O assumptions)
│   │   ├── macha-core.cabal
│   │   ├── Macha/
│   │   │   ├── Types.hs     # Tag, Fact, Application
│   │   │   ├── Database.hs  # SQLite operations
│   │   │   ├── LLM.hs       # Anthropic integration (future)
│   │   │   └── Match.hs     # Matching/evaluation logic (future)
│   │   └── test/
│   │       └── Spec.hs      # Unit tests
│   │
│   ├── macha-cli/           # CLI client
│   │   ├── macha-cli.cabal
│   │   ├── Main.hs
│   │   └── test/
│   │       └── Spec.hs      # CLI tests
│   │
│   ├── macha-api/           # REST API (future)
│   │   └── ...
│   │
│   └── macha-scraper/       # LinkedIn scraper (future)
│       └── ...
```

## Implementation Phases

### Phase 1: Foundation
- [ ] Project setup (cabal workspace, directory structure)
- [ ] Core types (Tag, Fact, Application)
- [ ] SQLite schema (2 entities, 2 join tables)
- [ ] Basic CRUD for all entities

### Phase 2: CLI
- [ ] CLI skeleton with optparse-applicative
- [ ] Fact commands (create, list, show, import)
- [ ] Tag commands (create, list)
- [ ] Application commands (apply, list)

### Phase 3: LLM Integration
- [ ] Anthropic API client
- [ ] Fact extraction from text/URL
- [ ] Match evaluation between facts
- [ ] Content generation (cover letters, Q&A)

### Phase 4: Scraper
- [ ] Playwright MCP integration
- [ ] LinkedIn job fetching
- [ ] Fact extraction from job postings

### Phase 5: API
- [ ] Servant REST API
- [ ] Same operations as CLI
- [ ] Network deployment

## Critical Files to Create

1. `src/macha-core/Macha/Types.hs` - Tag, Fact, Application types
2. `src/macha-core/Macha/Database.hs` - SQLite schema + operations
3. `src/macha-cli/Main.hs` - CLI entry point
4. `src/macha-core/Macha/LLM.hs` - Anthropic integration
5. `src/macha-core/Macha/Match.hs` - Fact matching/evaluation

## Initial Data

**Your profile (v5)** will be imported as facts:
- Path: `/home/ig/Downloads/ilka_guigova_profile_v5.txt`
- LLM extracts facts, tags them appropriately
- Creates root fact (profile) with child facts

## Verification Plan

1. **Unit tests**: Type serialization, database round-trips
2. **Integration tests**: LLM mock responses, full workflows
3. **Manual testing**:
   - Create root fact (user profile) + child facts
   - Import your profile v5
   - Fetch Kong job via scraper
   - Pair user facts with job facts (apply)
   - Generate cover letter

## Sample Workflow

```bash
# 1. Initialize
macha init

# 2. Import your profile (creates root fact + children)
macha fact import ~/Downloads/ilka_guigova_profile_v5.txt --tag user

# 3. Fetch job (creates root fact + children)
macha fact import "https://www.linkedin.com/jobs/view/4354585884" --tag job

# 4. Pair facts (apply)
macha apply <user-root-id> <job-root-id>

# 5. Evaluate match
macha match <user-root-id> <job-root-id>

# 6. Generate cover letter
macha generate cover-letter <user-root-id> <job-root-id>
```

## Notes

- **LinkedIn scraping**: Playwright MCP (authenticated browser session)
- **Fact ownership**: Child facts via owner_id, root facts have owner_id = NULL
- **Applications**: Root facts too, with cover letter/analysis as children
- **Application table**: Graph edges linking facts together
- **Storage**: `~/.macha/macha.db`
