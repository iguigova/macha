#!/usr/bin/env bash
# job_board.sh — one company's job board through its ATS's public API, one TSV
# row per posting: id, title, location, workplace, pay, url. Workplace and pay
# are what the board states and empty where it states nothing; Greenhouse lists
# neither, so its posting is read for them. The location joins every location
# the posting names with "; ".
#
# Given an id, it checks that one posting: the row and exit 0 while it is open,
# exit 1 once it is closed. A posting's 404 means closed only when its board
# answers; a board that answers 404 is under another ATS or another name, and
# exits 4, so a wrong board name never reads as a closed posting.
#
#   greenhouse  https://boards-api.greenhouse.io/v1/boards/<board>/jobs[/<id>]
#   ashby       https://api.ashbyhq.com/posting-api/job-board/<board>?includeCompensation=true
#               (no single-posting endpoint: an id is looked up in the board)
#   lever       https://api.lever.co/v0/postings/<board>[/<id>]
#
# Usage:
#   job_board.sh <greenhouse|ashby|lever> <board> [<id>]
#   job_board.sh --self-test   # checks the rows against fixtures, no network
#   job_board.sh --help
# Exit: 0 open, 1 closed, 2 usage, 3 unreachable or any other HTTP answer,
# 4 no such board.
set -euo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac

# rows <ats>: the API's JSON on stdin, a board or one posting, to TSV rows.
rows() {
  case "$1" in
    greenhouse) jq -r '(.jobs // [.])[]
      | [ .id, .title, .location.name, "", "", .absolute_url ] | @tsv' ;;
    ashby) jq -r '.jobs[]
      | [ .id, .title, ([.location] + [.secondaryLocations[]?.location] | join("; ")),
          (.workplaceType // ""), (.compensation.scrapeableCompensationSalarySummary // ""), .jobUrl ] | @tsv' ;;
    lever) jq -r '(if type == "array" then .[] else . end)
      | [ .id, .text, (.categories.allLocations // [.categories.location] | join("; ")),
          (.workplaceType // ""),
          (.salaryRange | if . then "\(.currency) \(.min)–\(.max) \(.interval)" else "" end),
          .hostedUrl ] | @tsv' ;;
  esac
}

url() {
  local ats=$1 board=$2 id=${3:-}
  case "$ats" in
    greenhouse) echo "https://boards-api.greenhouse.io/v1/boards/$board/jobs${id:+/$id}" ;;
    ashby)      echo "https://api.ashbyhq.com/posting-api/job-board/$board?includeCompensation=true" ;;
    lever)      echo "https://api.lever.co/v0/postings/$board${id:+/$id}" ;;
  esac
}

# get <url> [<body file>]: the HTTP code on stdout, 000 when curl got no answer,
# so an unreachable host is a verdict rather than curl's own exit.
get() { curl -sS -o "${2:-/dev/null}" -w '%{http_code}' "$1" || true; }

# not_found <id> <board's HTTP code>: the verdict on a 404, its line on stderr
# and its exit. Without an id, or with an Ashby id, the 404 was the board's own.
not_found() {
  local id=$1 board_code=$2
  if [[ -n "$id" && "$board_code" == 200 ]]; then echo "closed"; return 1; fi
  if [[ "$board_code" == 404 ]]; then echo "no board"; return 4; fi
  if [[ "$board_code" == 000 ]]; then echo "board unreachable"; return 3; fi
  echo "board answered HTTP $board_code"; return 3
}

if [[ "${1:-}" == "--self-test" ]]; then
  # shellcheck source=/dev/null
  . "$HOME/.claude/scripts/tap.sh"
  gh='{"id":7,"title":"Go Engineer","location":{"name":"Remote, Canada"},"absolute_url":"https://g.example/7"}'
  ash='{"jobs":[{"id":"a1","title":"SRE","location":"Toronto","secondaryLocations":[{"location":"Canada"}],"workplaceType":"Remote","compensation":{"scrapeableCompensationSalarySummary":"CA$150K - CA$190K"},"jobUrl":"https://a.example/a1"}]}'
  lev='{"id":"l1","text":"Backend","categories":{"location":"Vancouver","allLocations":["Vancouver","Remote"]},"workplaceType":"remote","salaryRange":{"currency":"CAD","min":140000,"max":180000,"interval":"per-year-salary"},"hostedUrl":"https://l.example/l1"}'
  grow=$'7\tGo Engineer\tRemote, Canada\t\t\thttps://g.example/7'
  lrow=$'l1\tBackend\tVancouver; Remote\tremote\tCAD 140000–180000 per-year-salary\thttps://l.example/l1'
  tap_is "a Greenhouse board gives a row with no workplace or pay" "$(rows greenhouse <<<"{\"jobs\":[$gh]}")" "$grow"
  tap_is "one Greenhouse posting gives the same row" "$(rows greenhouse <<<"$gh")" "$grow"
  tap_is "an Ashby board joins its locations and states workplace and pay" "$(rows ashby <<<"$ash")" \
    $'a1\tSRE\tToronto; Canada\tRemote\tCA$150K - CA$190K\thttps://a.example/a1'
  tap_is "a Lever board joins its locations and states workplace and pay" "$(rows lever <<<"[$lev]")" "$lrow"
  tap_is "one Lever posting gives the same row" "$(rows lever <<<"$lev")" "$lrow"
  tap_is "an Ashby id is looked up in its board, not in the URL" "$(url ashby x 9)" \
    "https://api.ashbyhq.com/posting-api/job-board/x?includeCompensation=true"
  verdict() { local v st=0; v=$(not_found "$1" "$2") || st=$?; echo "$v $st"; }
  tap_is "a posting's 404 under a board that answers is closed" "$(verdict 9 200)" "closed 1"
  tap_is "a posting's 404 under a board that 404s is no board" "$(verdict 9 404)" "no board 4"
  tap_is "a posting's 404 under a board erroring is never closed" "$(verdict 9 500)" "board answered HTTP 500 3"
  tap_is "a posting's 404 under an unreachable board is never closed" "$(verdict 9 000)" "board unreachable 3"
  tap_is "a 404 with no id is no board" "$(verdict '' 404)" "no board 4"
  tap_is "an unreachable host answers 000" "$(get http://127.0.0.1:9/ 2>/dev/null)" 000
  tap_end
fi

[[ $# -eq 2 || $# -eq 3 ]] && [[ "$1" =~ ^(greenhouse|ashby|lever)$ ]] \
  || { echo "Usage: $0 <greenhouse|ashby|lever> <board> [<id>]" >&2; exit 2; }
ats=$1 board=$2 id=${3:-}

body=$(mktemp); trap 'rm -f "$body"' EXIT
code=$(get "$(url "$ats" "$board" "$id")" "$body")
case "$code" in
  200) ;;
  404) board_code=404
       if [[ -n "$id" && "$ats" != ashby ]]; then
         board_code=$(get "$(url "$ats" "$board")")
       fi
       st=0; verdict=$(not_found "$id" "$board_code") || st=$?
       echo "$verdict: $ats $board${id:+ $id}" >&2; exit "$st" ;;
  000) echo "job_board.sh: $ats $board unreachable" >&2; exit 3 ;;
  *)   echo "job_board.sh: $ats $board answered HTTP $code" >&2; exit 3 ;;
esac

if [[ -n "$id" && "$ats" == ashby ]]; then
  rows ashby <"$body" | awk -F'\t' -v id="$id" '$1 == id { print; found = 1 } END { exit !found }' \
    || { echo "closed: $ats $board $id" >&2; exit 1; }
else
  rows "$ats" <"$body"
fi
