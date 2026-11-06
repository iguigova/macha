#!/usr/bin/env bash
# selftest.sh — the gate tooling_commit.sh runs for this repo: shell syntax and
# lint, each script's own --self-test, skill frontmatter and allowed-tools, and the
# names in prose changed since HEAD. A new script is covered the moment it lands.
# Exit 0 = clean, 1 = a check failed. Each failure prints a FAIL line naming it.
set -uo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac
cd "$(dirname "${BASH_SOURCE[0]}")/../.." || exit 2   # → the repo root
LINTS=~/.claude/scripts
fail=0

check() {  # <what> <command…> — run it; on failure show its output and a FAIL line
  local what="$1" out; shift
  if out=$("$@" 2>&1); then echo "  ok  $what"
  else printf '%s\n' "$out" | tail -20; echo "  FAIL $what"; fail=1; fi
}

mapfile -t scripts < <(find .claude/scripts -type f -not -name selftest.sh | sort)
mapfile -t prose < <({ git diff --name-only --diff-filter=ACMR HEAD -- '*.md'
                       git ls-files -o --exclude-standard -- '*.md'; } | sort -u)

for s in "${scripts[@]}" "${BASH_SOURCE[0]}"; do check "bash -n $s" bash -n "$s"; done
check "shellcheck" shellcheck -S warning -x "${scripts[@]}" "${BASH_SOURCE[0]}"
for s in "${scripts[@]}"; do check "$s --self-test" "$s" --self-test; done
check "skill frontmatter" "$LINTS/frontmatter_lint.py" .claude
check "allowed-tools rules" "$LINTS/allowed_tools_lint.py" .claude
[ "${#prose[@]}" -eq 0 ] || check "names in changed prose" "$LINTS/name_lint.sh" --gate "${prose[@]}"

[ "$fail" -eq 0 ] && echo "selftest: PASS" || echo "selftest: FAIL"
exit "$fail"
