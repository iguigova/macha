#!/usr/bin/env bash
# job_log.sh — every posting of one kind the session history names, one row
# each, at its latest outcome. The kind is `job` (the default) or `rfp`: a job
# is the topic `job: <Company> — <Role>`, an RFP `rfp: <Buyer> — <Title>`. Each
# entry under it opens with a bullet `<queued|applied|closed|skipped>: <url>`
# (README.md, The job log). The newest entry of a topic is its state, so a
# posting queued and then applied to is one `applied` row. Times are to the minute, so two entries
# in one minute are told apart by their place in the file, which only appends.
#
# A posting whose newest entry opens with any other bullet is an error, named,
# not a row left out: a posting missing from the log reads as one never seen.
#
# It reads through journal_scan.sh, the one parser of the history, so the format
# is never restated here.
#
# Usage:
#   job_log.sh [job|rfp]     # TSV: date (UTC), outcome, <Company> — <Role>, url
#   job_log.sh --self-test   # checks the rows against a scratch history
#   job_log.sh --help
set -euo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac

SCRIPTS="${CLAUDE_SCRIPTS:-$HOME/.claude/scripts}"

# rows <kind>
rows() {
  "$SCRIPTS/journal_scan.sh" | jq -r --arg kind "$1" '
    [ .no_ticket | to_entries[]
      | .key as $at | .value
      | select(.header | test("\\] — \($kind): "))
      | { at: [.ts, $at], ts, job: (.header | sub("^.*\\] — \($kind): "; "") | sub(" \\(via [^)]*\\)$"; "")),
          first: (.block | split("\n") | map(select(startswith("- "))) | first // "" | ltrimstr("- ")) } ]
    | group_by(.job) | map(max_by(.at)) | sort_by(.at)[]
    | . as $e
    | ([.first | capture("^(?<outcome>queued|applied|closed|skipped): (?<url>\\S+)$")] | first
       // error("job_log.sh: the newest entry of \($kind): \($e.job) opens with no <outcome>: <url> bullet")) as $f
    | [ .ts, $f.outcome, .job, $f.url ] | @tsv'
}

if [[ "${1:-}" == "--self-test" ]]; then
  fx=$(mktemp -d); trap 'rm -rf "$fx"' EXIT
  export JOURNAL_HIST="$fx/session_history.md"
  printf '# Log\n' > "$JOURNAL_HIST"; printf 'job\nrfp\n' > "$fx/topic_kinds"
  j() { "$SCRIPTS/journal.sh" "$@" >/dev/null; }
  j -t "job: Acme — Go Engineer" -b "queued: https://a.example/1" -b "fit"
  j -t "chore: unrelated" -b "applied: https://not.a.job/"
  j -t "job: Beta — SRE" -b "closed: https://b.example/2" -b "gone"
  j --via /job:apply -t "job: Acme — Go Engineer" -b "applied: https://a.example/1" -b "shown"
  j -t "rfp: Council — IT Services" -b "queued: https://c.example/3" -b "fit"
  got=$(rows job | cut -f2-)
  want=$'closed\tBeta — SRE\thttps://b.example/2\napplied\tAcme — Go Engineer\thttps://a.example/1'
  gotr=$(rows rfp | cut -f2-)
  wantr=$'queued\tCouncil — IT Services\thttps://c.example/3'
  j -t "job: Gamma — Dev" -b "applied to it"
  bad=$(rows job 2>&1 >/dev/null) && bad=""
  if [[ "$got" == "$want" && "$gotr" == "$wantr" && "$bad" == *"job: Gamma — Dev opens with no"* ]]; then
    echo "job_log self-test: PASS (one row per posting of the kind at its newest entry, the via suffix dropped, other topics skipped, a posting with no outcome bullet an error)"
    exit 0
  fi
  printf 'job_log self-test: FAIL\n  want: %s\n  got:  %s\n  want rfp: %s\n  got rfp:  %s\n  malformed: %s\n' "$want" "$got" "$wantr" "$gotr" "$bad" >&2
  exit 1
fi

kind="${1:-job}"
[[ $# -le 1 && ( "$kind" == job || "$kind" == rfp ) ]] || { echo "Usage: $0 [job|rfp | --self-test | --help]" >&2; exit 2; }
rows "$kind"
