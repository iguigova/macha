#!/usr/bin/env bash
# job_log.sh — every job the session history names, one row each, at its latest
# outcome. A job is the topic `job: <Company> — <Role>`; each entry under it
# opens with a bullet `<queued|applied|closed|skipped>: <url>` (README.md, The
# job log). The newest entry of a topic is the job's state, so a job queued and
# then applied to is one `applied` row. Times are to the minute, so two entries
# in one minute are told apart by their place in the file, which only appends.
#
# A job whose newest entry opens with any other bullet is an error, named, not a
# row left out: a job missing from the log reads as a job never seen.
#
# It reads through journal_scan.sh, the one parser of the history, so the format
# is never restated here.
#
# Usage:
#   job_log.sh               # TSV: date (UTC), outcome, <Company> — <Role>, url
#   job_log.sh --self-test   # checks the rows against a scratch history
#   job_log.sh --help
set -euo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac

SCRIPTS="${CLAUDE_SCRIPTS:-$HOME/.claude/scripts}"

rows() {
  "$SCRIPTS/journal_scan.sh" | jq -r '
    [ .no_ticket | to_entries[]
      | .key as $at | .value
      | select(.header | test("\\] — job: "))
      | { at: [.ts, $at], ts, job: (.header | sub("^.*\\] — job: "; "") | sub(" \\(via [^)]*\\)$"; "")),
          first: (.block | split("\n") | map(select(startswith("- "))) | first // "" | ltrimstr("- ")) } ]
    | group_by(.job) | map(max_by(.at)) | sort_by(.at)[]
    | . as $e
    | ([.first | capture("^(?<outcome>queued|applied|closed|skipped): (?<url>\\S+)$")] | first
       // error("job_log.sh: the newest entry of job: \($e.job) opens with no <outcome>: <url> bullet")) as $f
    | [ .ts, $f.outcome, .job, $f.url ] | @tsv'
}

if [[ "${1:-}" == "--self-test" ]]; then
  fx=$(mktemp -d); trap 'rm -rf "$fx"' EXIT
  export JOURNAL_HIST="$fx/session_history.md"
  printf '# Log\n' > "$JOURNAL_HIST"; printf 'job\n' > "$fx/topic_kinds"
  j() { "$SCRIPTS/journal.sh" "$@" >/dev/null; }
  j -t "job: Acme — Go Engineer" -b "queued: https://a.example/1" -b "fit"
  j -t "chore: unrelated" -b "applied: https://not.a.job/"
  j -t "job: Beta — SRE" -b "closed: https://b.example/2" -b "gone"
  j --via /job:apply -t "job: Acme — Go Engineer" -b "applied: https://a.example/1" -b "shown"
  got=$(rows | cut -f2-)
  want=$'closed\tBeta — SRE\thttps://b.example/2\napplied\tAcme — Go Engineer\thttps://a.example/1'
  j -t "job: Gamma — Dev" -b "applied to it"
  bad=$(rows 2>&1 >/dev/null) && bad=""
  if [[ "$got" == "$want" && "$bad" == *"job: Gamma — Dev opens with no"* ]]; then
    echo "job_log self-test: PASS (one row per job at its newest entry, the via suffix dropped, non-job topics skipped, a job with no outcome bullet an error)"
    exit 0
  fi
  printf 'job_log self-test: FAIL\n  want: %s\n  got:  %s\n  malformed: %s\n' "$want" "$got" "$bad" >&2
  exit 1
fi

[[ $# -eq 0 ]] || { echo "Usage: $0 [--self-test | --help]" >&2; exit 2; }
rows
