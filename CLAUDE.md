# Macha

Job applications for one person, generated from `jobs/profile/profile.txt`.
README.md states the commands and how the profile changes.

- Facts about the user live only in `profile.txt`, and who the user is
  in `~/.claude/credo.md`. Read them there; never copy a fact into
  another file.
- `out/` is scratch for the current application and is never committed.
- A note captured under a `job: ` topic is a queued job, and `/job:apply`
  is the command that does it and closes it. README.md states the job log.
- A note captured under an `rfp: ` topic is a queued RFP. Jobs and RFPs
  take the same outcomes; README.md states them.
