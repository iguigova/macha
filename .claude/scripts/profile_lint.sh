#!/usr/bin/env bash
# profile_lint.sh — the profile lines that match one of three shapes, one row
# each: line number, rule, the line. It reports; the integrity check judges
# each row, since a rule matches a pattern, not a meaning. Facts joined into
# a list by commas match no rule; reading the profile finds those.
#
#   sentences     two sentences on one line. Quoted text and the
#                 abbreviations B.Sc., St., Inc. and e.g. are skipped.
#   clauses       a second clause joined by "and": ", and I", " and am".
#   professional  "professional" on an experience line: "experience" or "I
#                 have used". It reads as a limit, by the paid-work rule in
#                 .claude/skills/job:apply/steps.md (Evidence).
#
# Usage:
#   profile_lint.sh [<profile>]   # default: jobs/profile/profile.txt
#   profile_lint.sh --self-test   # checks the rules against a fixture
#   profile_lint.sh --help
# Exit: 0 nothing found, 1 rows printed, 2 usage.
set -euo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac

# lint <file>: one TSV row per hit.
lint() {
  awk -v OFS='\t' '
    { t = $0
      gsub(/"[^"]*"/, "", t)
      gsub(/B\.Sc\.|St\.|Inc\.|e\.g\./, "", t) }
    t ~ /[a-z0-9)%]\. [A-Z]/                  { print NR, "sentences", $0 }
    t ~ /, and I |[^,] and am /               { print NR, "clauses", $0 }
    t ~ /[Pp]rofessional/ && t ~ /experience|I have used/ { print NR, "professional", $0 }
  ' "$1"
}

self_test() {
  local f out want
  f=$(mktemp); trap 'rm -f "$f"' RETURN
  cat > "$f" <<'EOF'
I have 3+ years of C++ experience.
I hold no clearance and am eligible for one.
I built a thing. I also built another.
I describe myself as: "A kernel. Of laughter."
I have a B.Sc. Mathematics from St. Andrews.
I have 2 years of professional Go experience.
I have worked professionally in 10+ languages.
I have used Python professionally since 2026.
I wrote the client, and I built the server.
EOF
  want=$'2\tclauses\n3\tsentences\n6\tprofessional\n8\tprofessional\n9\tclauses'
  out=$(lint "$f" | cut -f1,2 | sort -n)
  if [[ "$out" == "$want" ]]; then echo "self-test: ok"; else
    echo "self-test: FAIL"; diff <(echo "$want") <(echo "$out") || true; return 1; fi
}

case "${1:-}" in
  --self-test) self_test; exit ;;
  -*) echo "unknown option: $1" >&2; exit 2 ;;
esac
profile=${1:-$(dirname "$0")/../../jobs/profile/profile.txt}
[[ -r "$profile" ]] || { echo "no profile: $profile" >&2; exit 2; }
rows=$(lint "$profile")
[[ -z "$rows" ]] && exit 0
echo "$rows"; exit 1
