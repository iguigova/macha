#!/usr/bin/env bash
# selftest.sh — the gate tooling_commit.sh runs for this repo: shell syntax and
# lint, each script's own --self-test, held to TAP as tap.sh states it, skill
# frontmatter and allowed-tools, and the names in prose changed since HEAD. A new
# script is covered the moment it lands.
# Exit 0 = clean, 1 = a check failed. Each failure prints a FAIL line naming it.
set -uo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac
cd "$(dirname "${BASH_SOURCE[0]}")/../.." || exit 2   # → the repo root
LINTS=~/.claude/scripts
# shellcheck source=/dev/null
. "$LINTS/tap.sh"
fail=0

check() {  # <what> <command…> — run it; on failure show its output and a FAIL line
  local what="$1" out; shift
  if out=$("$@" 2>&1); then echo "  ok  $what"
  else printf '%s\n' "$out" | tail -20; echo "  FAIL $what"; fail=1; fi
}

self_test_holds() {  # <script> — exits 0, every case ok, and nothing on stdout but TAP
  local tap st=0 wrong
  tap=$(mktemp)
  "$1" --self-test >"$tap" || st=$?
  wrong=$(tap_verify "$tap")
  if [ "$st" -eq 0 ] && [ -z "$wrong" ] && ! grep -q '^not ok' "$tap"; then
    rm -f "$tap"; return 0
  fi
  cat "$tap"; rm -f "$tap"
  [ -z "$wrong" ] || echo "does not print TAP: $wrong"
  echo "exit $st"
  return 1
}

mapfile -t scripts < <(find .claude/scripts -type f -not -name selftest.sh | sort)
mapfile -t prose < <({ git diff --name-only --diff-filter=ACMR HEAD -- '*.md'
                       git ls-files -o --exclude-standard -- '*.md'; } | sort -u)

for s in "${scripts[@]}" "${BASH_SOURCE[0]}"; do check "bash -n $s" bash -n "$s"; done
check "shellcheck" shellcheck -S warning -x "${scripts[@]}" "${BASH_SOURCE[0]}"
for s in "${scripts[@]}"; do check "$s --self-test" self_test_holds "$s"; done
check "skill frontmatter" "$LINTS/frontmatter_lint.py" .claude
check "allowed-tools rules" "$LINTS/allowed_tools_lint.py" .claude
[ "${#prose[@]}" -eq 0 ] || check "names in changed prose" "$LINTS/name_lint.sh" --gate "${prose[@]}"

[ "$fail" -eq 0 ] && echo "selftest: PASS" || echo "selftest: FAIL"
exit "$fail"
