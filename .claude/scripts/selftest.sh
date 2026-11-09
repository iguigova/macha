#!/usr/bin/env bash
# selftest.sh — the gate tooling_commit.sh runs for this repo: gate_lib.sh's checks
# over .claude. A new script is covered the moment it lands.
# Exit 0 = clean, 1 = a check failed. Each failure prints a FAIL line naming it.
#
# Usage:
#   selftest.sh
#   selftest.sh --self-test   a copy of this gate on a fixture repo fails each break
set -uo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac
# shellcheck source=/dev/null
. ~/.claude/scripts/gate_lib.sh

if [ "${1:-}" = --self-test ]; then
  # A copy of this gate in a fixture repo, run with GATE_SELF_TEST set so it does
  # not sweep itself. Each case breaks one thing, and the gate must fail naming it.
  fx=$(mktemp -d); trap 'rm -rf "$fx"' EXIT
  mkdir -p "$fx/.claude/scripts"; cp "${BASH_SOURCE[0]}" "$fx/.claude/scripts/selftest.sh"
  echo '{}' > "$fx/.claude/settings.json"
  printf '#!/usr/bin/env bash\n# offers --self-test\nprintf "ok 1 - x\\n1..1\\n"\n' > "$fx/.claude/scripts/clean.sh"
  git -C "$fx" init -q
  git -C "$fx" add -A && git -C "$fx" -c user.email=t@t -c user.name=t commit -qm base
  gate() { GATE_SELF_TEST=1 bash "$fx/.claude/scripts/selftest.sh" 2>&1; }
  out=$(gate); st=$?
  tap_is "a clean repo passes" "$st $(tail -1 <<<"$out")" "0 selftest: PASS"
  printf 'if\n' > "$fx/.claude/scripts/broken.sh"
  tap_like "a syntax error fails the gate, naming the script" "$(gate)" \
    "*FAIL bash -n: .claude/scripts/broken.sh*selftest: FAIL"
  rm "$fx/.claude/scripts/broken.sh"
  printf '#!/usr/bin/env bash\n# offers --self-test\nprintf "not ok 1 - x\\n1..1\\n"; exit 1\n' > "$fx/.claude/scripts/red.sh"
  tap_like "a failing self-test fails the gate, naming the script" "$(gate)" \
    "*FAIL .claude/scripts/red.sh --self-test*selftest: FAIL"
  rm "$fx/.claude/scripts/red.sh"
  mkdir -p "$fx/.claude/skills/x"; printf -- '---\nname: [\n---\nbody\n' > "$fx/.claude/skills/x/SKILL.md"
  tap_like "a malformed skill frontmatter fails the gate" "$(gate)" "*FAIL frontmatter_lint.py exited*selftest: FAIL"
  rm -r "$fx/.claude/skills"
  printf 'run `.claude/scripts/no_such_script_here.sh` first\n' > "$fx/notes.md"
  tap_like "changed prose naming a missing script fails the gate" "$(gate)" "*FAIL name_lint.sh exited*selftest: FAIL"
  rm "$fx/notes.md"
  out=$(gate); tap_is "the repo passes again once each break is undone" "$(tail -1 <<<"$out")" "selftest: PASS"
  tap_end
fi

cd "$(dirname "${BASH_SOURCE[0]}")/../.." || exit 2   # → the repo root

echo "== syntax ==";               gate_syntax .claude/scripts/*
echo "== shellcheck ==";           gate_shellcheck .claude/scripts/*
echo "== self-tests ==";           gate_self_tests .claude/scripts
echo "== skills and commands ==";  gate_run "$GATE_SCRIPTS/frontmatter_lint.py" .claude
                                   gate_run "$GATE_SCRIPTS/allowed_tools_lint.py" .claude
echo "== permission rules ==";     gate_run "$GATE_SCRIPTS/settings_lint.py" .claude/settings.json
echo "== names in changed prose =="; gate_names_changed .
gate_end
