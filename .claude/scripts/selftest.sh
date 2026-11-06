#!/usr/bin/env bash
# selftest.sh — the gate tooling_commit.sh runs for this repo: gate_lib.sh's checks
# over .claude. A new script is covered the moment it lands.
# Exit 0 = clean, 1 = a check failed. Each failure prints a FAIL line naming it.
set -uo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac
cd "$(dirname "${BASH_SOURCE[0]}")/../.." || exit 2   # → the repo root
# shellcheck source=/dev/null
. ~/.claude/scripts/gate_lib.sh

echo "== syntax ==";               gate_syntax .claude/scripts/*
echo "== shellcheck ==";           gate_shellcheck .claude/scripts/*
echo "== self-tests ==";           gate_self_tests .claude/scripts
echo "== skills and commands ==";  gate_run "$GATE_SCRIPTS/frontmatter_lint.py" .claude
                                   gate_run "$GATE_SCRIPTS/allowed_tools_lint.py" .claude
echo "== names in changed prose =="; gate_names_changed .
gate_end
