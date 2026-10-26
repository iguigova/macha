#!/usr/bin/env bash
# rfp_board.sh — the open notices of one public-procurement source, one TSV row
# per notice: id, title, closing, buyer, method, url. Closing is ISO, to the
# minute where the source gives a time and the day where it gives only a date,
# in the source's own zone. A notice closing before today is dropped, since a
# source's own "open" can outlive its closing date. Buyer and method are empty
# where the source states nothing; method is the procurement method, which says
# whether anyone may bid (CanadaBuys), or the opportunity type (marketplace).
#
# Given an id, it checks that one notice: the row and exit 0 while it is open,
# exit 1 once it is not.
#
#   canadabuys  https://canadabuys.canada.ca/opendata/pub/openTenderNotice-ouvertAvisAppelOffres.csv
#               federal notices only, refreshed daily; id is the reference number.
#               A row the CSV gives no URL is linked by its reference when that is
#               a cb- one, the site's own form; any other such row's url is empty
#   marketplace https://marketplace.digital.gov.bc.ca/api/opportunities/<type>
#               the three types, open when PUBLISHED; id is <type>/<uuid>
#   bonfire     https://<org>.bonfirehub.ca/PublicPortal/getOpenPublicOpportunitiesSectionData
#               one organisation's portal; the buyer is the org; a notice's page
#               opens in a browser only, behind a Cloudflare check
#   civicinfo   https://www.civicinfo.bc.ca/rss/bids
#               recent BC local-government postings, not every open one
#
# Usage:
#   rfp_board.sh <canadabuys|marketplace|civicinfo> [<id>]
#   rfp_board.sh bonfire <org> [<id>]
#   rfp_board.sh --self-test   # checks the rows against fixtures, no network
#   rfp_board.sh --help
# Exit: 0 open, 1 not open or no such portal, 2 usage, 3 unreachable or any other HTTP answer.
set -euo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac

MP=https://marketplace.digital.gov.bc.ca
MP_TYPES=(code-with-us sprint-with-us team-with-us)

# rows <source> [<org>|<type>]: the source's body on stdin to TSV rows, every
# notice. A marketplace list names no type, so its caller passes the one it read.
rows() {
  case "$1" in
    canadabuys) python3 -I -c '
import csv, io, sys
for r in csv.DictReader(io.TextIOWrapper(sys.stdin.buffer, encoding="utf-8-sig")):
    k = lambda p: next((v for c, v in r.items() if c.startswith(p)), "").strip()
    ref, url = k("referenceNumber"), k("noticeURL-URLavis-eng")
    if not url and ref.startswith("cb-"):
        url = "https://canadabuys.canada.ca/en/tender-opportunities/tender-notice/" + ref
    print("\t".join(" ".join(v.split()) for v in [
        ref, k("title-titre-eng"), k("tenderClosingDate")[:16],
        k("contractingEntityName-nomEntitContractante-eng"),
        k("procurementMethod-methodeApprovisionnement-eng"), url]))' ;;
    marketplace) jq -r --arg mp "$MP" --arg t "$2" '.[] | select(.status == "PUBLISHED")
      | [ "\($t)/\(.id)", .title, (.proposalDeadline // "" | .[0:16]), "", $t,
          "\($mp)/opportunities/\($t)/\(.id)" ] | @tsv' ;;
    bonfire) jq -r --arg org "$2" '.payload.projects | (if type == "object" then [.[]] else . end)[]
      | [ .ProjectID, .ProjectName, (.DateClose // "" | sub(" "; "T") | .[0:16]), $org, "",
          "https://\($org).bonfirehub.ca/opportunities/\(.ProjectID)" ] | @tsv' ;;
    civicinfo) python3 -I -c '
import re, sys, html, datetime, xml.etree.ElementTree as ET
for it in ET.fromstring(sys.stdin.read().strip()).iter("item"):
    title, link = it.findtext("title", ""), it.findtext("link", "")
    name, buyer, depth = title.rstrip(), "", 0
    if name.endswith(")"):
        for i in range(len(name) - 1, -1, -1):
            depth += {")": 1, "(": -1}.get(name[i], 0)
            if depth == 0:
                name, buyer = name[:i].strip(), name[i + 1:-1].strip()
                break
    d = re.search(r"Posting expires: \w+, (\w+ \d+, \d{4})", html.unescape(it.findtext("description", "")))
    closing = datetime.datetime.strptime(d.group(1), "%B %d, %Y").date().isoformat() if d else ""
    print("\t".join([link.rsplit("=", 1)[-1], name, closing, buyer, "", link]))' ;;
  esac
}

# live TODAY: the rows closing on or after TODAY, or stating no closing.
live() { awk -F'\t' -v today="$1" '$3 == "" || substr($3, 1, 10) >= today'; }

if [[ "${1:-}" == "--self-test" ]]; then
  cb=$'\xef\xbb\xbf"title-titre-eng","referenceNumber-numeroReference","tenderClosingDate-appelOffresDateCloture","contractingEntityName-nomEntitContractante-eng","procurementMethod-methodeApprovisionnement-eng","noticeURL-URLavis-eng"\n'
  cb+=$'"IT Services","cb-1","2026-11-23T14:00:00","Council","Competitive - Traditional",""\n'
  cb+=$'"Old","cb-2","2025-08-26T16:00:00","Agency","Competitive - Open bidding","https://c.example/2"\n'
  cb+=$'"Data Hub","WS9-Doc9","2026-11-13T14:00:00","PSPC","Competitive - Open bidding",""\n'
  mp='[{"id":"u1","title":"Fix it","status":"PUBLISHED","proposalDeadline":"2026-11-02T23:59:00.000Z"},{"id":"u2","title":"Done","status":"AWARDED","proposalDeadline":"2026-01-01T00:00:00.000Z"}]'
  bf='{"payload":{"projects":{"9":{"ProjectID":"9","ProjectName":"Portal Build","DateClose":"2026-12-01 22:00:00"}}}}'
  ci='<?xml version="1.0"?><rss><channel><item><title>Web Site (Village of X)</title><link>http://www.civicinfo.bc.ca/bids?bidid=11048</link><description>Posting expires: Monday, November 9, 2026.</description></item><item><title>Snow Clearing (Vernon (City))</title><link>http://www.civicinfo.bc.ca/bids?bidid=11047</link><description>Request for Proposals&lt;br/&gt;Posting expires: Tuesday, November 17, 2026.&lt;br&gt;</description></item></channel></rss>'
  got=$(rows canadabuys <<<"$cb" | live 2026-10-26; rows marketplace code-with-us <<<"$mp"; rows bonfire ubc <<<"$bf"; rows civicinfo <<<"$ci")
  want=$'cb-1\tIT Services\t2026-11-23T14:00\tCouncil\tCompetitive - Traditional\thttps://canadabuys.canada.ca/en/tender-opportunities/tender-notice/cb-1\n'
  want+=$'WS9-Doc9\tData Hub\t2026-11-13T14:00\tPSPC\tCompetitive - Open bidding\t\n'
  want+=$'code-with-us/u1\tFix it\t2026-11-02T23:59\t\tcode-with-us\thttps://marketplace.digital.gov.bc.ca/opportunities/code-with-us/u1\n'
  want+=$'9\tPortal Build\t2026-12-01T22:00\tubc\t\thttps://ubc.bonfirehub.ca/opportunities/9\n'
  want+=$'11048\tWeb Site\t2026-11-09\tVillage of X\t\thttp://www.civicinfo.bc.ca/bids?bidid=11048\n'
  want+=$'11047\tSnow Clearing\t2026-11-17\tVernon (City)\t\thttp://www.civicinfo.bc.ca/bids?bidid=11047'
  if [[ "$got" == "$want" ]]; then
    echo "rfp_board self-test: PASS (each source to rows; a BOM-led CSV read; a cb- notice linked by its reference; a notice past its closing and one not PUBLISHED dropped; a buyer in nested brackets split off whole)"
    exit 0
  fi
  printf 'rfp_board self-test: FAIL\n  want:\n%s\n  got:\n%s\n' "$want" "$got" >&2
  exit 1
fi

src=${1:-}
case "$src" in
  canadabuys|marketplace|civicinfo) [[ $# -le 2 ]] && org="" id=${2:-} ;;
  bonfire) [[ $# -ge 2 && $# -le 3 ]] && org=$2 id=${3:-} ;;
  *) false ;;
esac || { echo "Usage: $0 <canadabuys|marketplace|civicinfo> [<id>] | bonfire <org> [<id>]" >&2; exit 2; }

# fetch URL: the body on stdout; an HTTP answer other than 200 exits.
fetch() {
  local body code; body=$(mktemp)
  code=$(curl -sS -A 'Mozilla/5.0' -o "$body" -w '%{http_code}' "$1")
  case "$code" in
    200) cat "$body"; rm -f "$body" ;;
    404) rm -f "$body"; echo "no such portal: $1" >&2; exit 1 ;;
    000) rm -f "$body"; echo "rfp_board.sh: could not reach $1" >&2; exit 3 ;;
    *)   rm -f "$body"; echo "rfp_board.sh: $1 answered HTTP $code" >&2; exit 3 ;;
  esac
}

all=$(case "$src" in
  canadabuys)  fetch https://canadabuys.canada.ca/opendata/pub/openTenderNotice-ouvertAvisAppelOffres.csv | rows canadabuys ;;
  marketplace) for t in "${MP_TYPES[@]}"; do fetch "$MP/api/opportunities/$t" | rows marketplace "$t"; done ;;
  bonfire)     fetch "https://$org.bonfirehub.ca/PublicPortal/getOpenPublicOpportunitiesSectionData" | rows bonfire "$org" ;;
  civicinfo)   fetch https://www.civicinfo.bc.ca/rss/bids | rows civicinfo ;;
esac | live "$(date +%F)")

if [[ -n "$id" ]]; then
  awk -F'\t' -v id="$id" '$1 == id { print; found = 1 } END { exit !found }' <<<"$all" \
    || { echo "not open: $src ${org:+$org }$id" >&2; exit 1; }
else
  [[ -z "$all" ]] || printf '%s\n' "$all"
fi
