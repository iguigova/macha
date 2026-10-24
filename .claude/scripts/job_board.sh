#!/usr/bin/env bash
# job_board.sh — one company's job board through its ATS's public API, one TSV
# row per posting: id, title, location, workplace, pay, url. Workplace and pay
# are what the board states and empty where it states nothing; Greenhouse lists
# neither, so its posting is read for them. The location joins every location
# the posting names with "; ".
#
# Given an id, it checks that one posting: the row and exit 0 while it is open,
# exit 1 once it is closed. A board that answers 404 exits 1 too: it is under
# another ATS or another name.
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
# Exit: 0 open, 1 closed or no such board, 2 usage, 3 any other HTTP answer.
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

if [[ "${1:-}" == "--self-test" ]]; then
  gh='{"jobs":[{"id":7,"title":"Go Engineer","location":{"name":"Remote, Canada"},"absolute_url":"https://g.example/7"}]}'
  ash='{"jobs":[{"id":"a1","title":"SRE","location":"Toronto","secondaryLocations":[{"location":"Canada"}],"workplaceType":"Remote","compensation":{"scrapeableCompensationSalarySummary":"CA$150K - CA$190K"},"jobUrl":"https://a.example/a1"}]}'
  lev='{"id":"l1","text":"Backend","categories":{"location":"Vancouver","allLocations":["Vancouver","Remote"]},"workplaceType":"remote","salaryRange":{"currency":"CAD","min":140000,"max":180000,"interval":"per-year-salary"},"hostedUrl":"https://l.example/l1"}'
  got=$(rows greenhouse <<<"$gh"; rows ashby <<<"$ash"; rows lever <<<"[$lev]"; rows lever <<<"$lev")
  want=$'7\tGo Engineer\tRemote, Canada\t\t\thttps://g.example/7\n'
  want+=$'a1\tSRE\tToronto; Canada\tRemote\tCA$150K - CA$190K\thttps://a.example/a1\n'
  lrow=$'l1\tBackend\tVancouver; Remote\tremote\tCAD 140000–180000 per-year-salary\thttps://l.example/l1'
  want+="$lrow"$'\n'"$lrow"
  if [[ "$got" == "$want" && "$(url ashby x 9)" != *9* ]]; then
    echo "job_board self-test: PASS (a board and one posting to rows for each ATS; an Ashby id looked up in its board)"
    exit 0
  fi
  printf 'job_board self-test: FAIL\n  want:\n%s\n  got:\n%s\n' "$want" "$got" >&2
  exit 1
fi

[[ $# -eq 2 || $# -eq 3 ]] && [[ "$1" =~ ^(greenhouse|ashby|lever)$ ]] \
  || { echo "Usage: $0 <greenhouse|ashby|lever> <board> [<id>]" >&2; exit 2; }
ats=$1 board=$2 id=${3:-}

body=$(mktemp); trap 'rm -f "$body"' EXIT
code=$(curl -sS -o "$body" -w '%{http_code}' "$(url "$ats" "$board" "$id")")
case "$code" in
  200) ;;
  404) if [[ -n "$id" ]]; then echo "closed: $ats $board $id" >&2; else echo "no board: $ats $board" >&2; fi; exit 1 ;;
  *)   echo "job_board.sh: $ats $board answered HTTP $code" >&2; exit 3 ;;
esac

if [[ -n "$id" && "$ats" == ashby ]]; then
  rows ashby <"$body" | awk -F'\t' -v id="$id" '$1 == id { print; found = 1 } END { exit !found }' \
    || { echo "closed: $ats $board $id" >&2; exit 1; }
else
  rows "$ats" <"$body"
fi
