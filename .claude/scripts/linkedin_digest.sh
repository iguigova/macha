#!/usr/bin/env bash
# linkedin_digest.sh — run /linkedin:digest unattended once a day, and say
# when a run did not send.
#
# `install` writes a systemd user timer, linkedin-digest.timer, at 07:00 local
# time with Persistent=true, so a morning the machine missed runs once it is
# next active. The service's PATH holds the directory of the playwright
# server's command (npx) as `install` finds it, since a timer reads no shell
# profile: a new node version is reinstalled. Each run starts
# `claude -p /linkedin:digest` in this repo with the playwright server
# ~/.claude.json configures plus --headless, since a timer has no display, and
# the skill's tools allowed. A run is sent when digest_mail.py rewrote
# .claude/linkedin_digest.last during it; any other end (no server command, an
# expired login, a refused password, a failed claude) opens one journal note
# naming the run's last "Not sent: <reason>" line, or its last line when it
# printed none. The run appends to .claude/linkedin_digest.log.
#
# Usage:
#   linkedin_digest.sh run        # what the timer runs
#   linkedin_digest.sh install    # write and enable the units
#   linkedin_digest.sh status     # when the timer last ran and next runs, and the last send
#   linkedin_digest.sh --self-test
#
# CLAUDE_BIN, CLAUDE_JSON, JOURNAL and XDG_CONFIG_HOME exist for the self-test.
set -uo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
UNIT=linkedin-digest
TOOLS="mcp__playwright__browser_navigate mcp__playwright__browser_snapshot mcp__playwright__browser_evaluate mcp__playwright__browser_close Read Write Bash(date:*) Bash(.claude/scripts/digest_mail.py:*)"

unit_dir() { echo "${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user"; }

server_command() {
  jq -er '.mcpServers.playwright.command' "${CLAUDE_JSON:-$HOME/.claude.json}"
}

service_text() {
  cat <<EOF
[Unit]
Description=Mail the LinkedIn feed's ranked digest (linkedin_digest.sh run)

[Service]
Type=oneshot
Environment=PATH=%h/.local/bin:$1:/usr/local/bin:/usr/bin:/bin
ExecStart=$HERE/linkedin_digest.sh run
EOF
}

timer_text() {
  cat <<EOF
[Unit]
Description=Daily LinkedIn digest at 07:00, caught up after a missed morning

[Timer]
OnCalendar=*-*-* 07:00:00
Persistent=true

[Install]
WantedBy=timers.target
EOF
}

install_units() {
  local d cmd bin; d=$(unit_dir)
  cmd=$(server_command) || { echo "no playwright server in ${CLAUDE_JSON:-$HOME/.claude.json}" >&2; return 1; }
  bin=$(command -v "$cmd") || { echo "the playwright server's command $cmd is not on PATH" >&2; return 1; }
  mkdir -p "$d" || return 1
  service_text "$(dirname "$bin")" > "$d/$UNIT.service" && timer_text > "$d/$UNIT.timer" || return 1
  systemctl --user daemon-reload && systemctl --user enable --now "$UNIT.timer" || return 1
  echo "installed $d/$UNIT.timer"
}

run_digest() {
  local log="$ROOT/.claude/linkedin_digest.log" stamp="$ROOT/.claude/linkedin_digest.last"
  local cfg mark rc=0 last start cmd
  cfg=$(mktemp); mark=$(mktemp); trap 'rm -f "$cfg" "$mark"' RETURN
  cd "$ROOT" || return 1
  start=$(( $([[ -e "$log" ]] && wc -l < "$log" || echo 0) + 1 ))
  echo "== $(date -u +%FT%TZ) run started" >> "$log"
  if ! cmd=$(server_command 2>> "$log") || ! jq -e '.mcpServers.playwright | .args += ["--headless"] | {mcpServers: {playwright: .}}' \
      "${CLAUDE_JSON:-$HOME/.claude.json}" > "$cfg" 2>> "$log"; then
    echo "Not sent: no playwright server in ${CLAUDE_JSON:-$HOME/.claude.json}" >> "$log"; rc=1
  elif ! command -v "$cmd" > /dev/null; then
    echo "Not sent: the playwright server's command $cmd is not on PATH; rerun linkedin_digest.sh install" >> "$log"; rc=1
  else
    # shellcheck disable=SC2086  # TOOLS is a list of tool names
    "${CLAUDE_BIN:-claude}" -p "/linkedin:digest" --mcp-config "$cfg" --strict-mcp-config \
      --allowedTools $TOOLS >> "$log" 2>&1 || rc=$?
  fi
  if [[ -e "$stamp" && "$stamp" -nt "$mark" ]]; then
    echo "== sent: $(cat "$stamp")" >> "$log"; return 0
  fi
  last=$(tail -n +"$start" "$log" | grep '^Not sent: ' | tail -n 1)
  [[ -n "$last" ]] || last=$(tail -n +"$start" "$log" | grep -v '^[[:space:]]*$' | grep -v '^== ' | tail -n 1)
  echo "== not sent (claude exit $rc)" >> "$log"
  env -u CLAUDE_CODE_SESSION_ID "${JOURNAL:-$HOME/.claude/scripts/journal.sh}" \
    -t "chore: linkedin digest" \
    -b "The $(date +%F) LinkedIn digest was not sent (claude exit $rc)." \
    -n "LinkedIn digest not sent — $(date +%F): ${last:-no output}; see .claude/linkedin_digest.log" \
    >> "$log" 2>&1
  return 1
}

self_test() {
  # shellcheck source=/dev/null
  . ~/.claude/scripts/tap.sh
  local fx; fx=$(mktemp -d); trap 'rm -rf "$fx"' EXIT
  mkdir -p "$fx/bin" "$fx/repo/.claude/scripts"
  cp "$HERE/linkedin_digest.sh" "$fx/repo/.claude/scripts/"
  local me="$fx/repo/.claude/scripts/linkedin_digest.sh"
  server() { printf '{"mcpServers":{"playwright":{"command":"%s","args":["-y","@playwright/mcp","--isolated"]}}}' "$1" > "$fx/claude.json"; }
  server fake-npx
  # A stand-in claude: records its arguments and the MCP config, then does $FAKE.
  cat > "$fx/bin/claude" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$FX/args"
while [[ $# -gt 0 ]]; do [[ "$1" == --mcp-config ]] && cp "$2" "$FX/mcp"; shift; done
case "$FAKE" in
  send) sleep 1; echo "2026-11-08 07:00 -0700 sent: Feed title" > .claude/linkedin_digest.last ;;
  wall) echo "Not sent: LinkedIn login wall; run playwright_login.sh"; echo "Nothing changed state."; exit 0 ;;
esac
EOF
  printf '#!/usr/bin/env bash\nprintf "%%s\\n" "$@" > "$FX/journal"\n' > "$fx/bin/journal"
  printf '#!/usr/bin/env bash\necho "$*" >> "$FX/systemctl.log"\n' > "$fx/bin/systemctl"
  printf '#!/usr/bin/env bash\n' > "$fx/bin/fake-npx"
  chmod +x "$fx/bin/"*
  go() { FX=$fx FAKE=$1 PATH="$fx/bin:$PATH" CLAUDE_BIN="$fx/bin/claude" CLAUDE_JSON="$fx/claude.json" \
         JOURNAL="$fx/bin/journal" "$me" run; }

  go send
  tap_is "a run that sent exits 0 and opens no note" "$?$( [[ -e $fx/journal ]] && echo note)" 0
  tap_is "claude runs the skill with the playwright server headless" \
    "$(jq -c '.mcpServers.playwright.args' "$fx/mcp")" '["-y","@playwright/mcp","--isolated","--headless"]'
  tap_is "claude runs only that server and the skill's tools" \
    "$(grep -qx -- --strict-mcp-config "$fx/args" && grep -qxF 'Bash(.claude/scripts/digest_mail.py:*)' "$fx/args" && echo y)" y
  tap_is "the log records the send" "$(tail -n 1 "$fx/repo/.claude/linkedin_digest.log")" \
    "== sent: 2026-11-08 07:00 -0700 sent: Feed title"

  go wall
  tap_is "a run that did not send exits 1" "$?" 1
  tap_is "its note names the run's Not sent line, not its last line" \
    "$(grep -c 'LinkedIn digest not sent — .*login wall' "$fx/journal")$(grep -c 'Nothing changed' "$fx/journal")" 10

  rm -f "$fx/journal" "$fx/args"; server no-such-command-here
  go send
  tap_is "with the server's command off PATH it does not run claude, and notes it" \
    "$?$([[ -e $fx/args ]] && echo ran)$(grep -c 'no-such-command-here is not on PATH' "$fx/journal")" 11

  printf '{"mcpServers":{}}' > "$fx/claude.json"; rm -f "$fx/journal"
  go send
  tap_is "with no playwright server it does not run claude, and notes it" \
    "$?$(grep -c 'no playwright server' "$fx/journal")" 11

  local out
  server fake-npx
  if out=$(FX=$fx PATH="$fx/bin:$PATH" CLAUDE_JSON="$fx/claude.json" XDG_CONFIG_HOME="$fx/cfg" install_units 2>&1); then
    tap_ok "the units install"
  else tap_not_ok "the units install" "$out"; fi
  tap_run "the service's PATH holds the server command's directory" \
    grep -qx "Environment=PATH=%h/.local/bin:$fx/bin:/usr/local/bin:/usr/bin:/bin" "$fx/cfg/systemd/user/$UNIT.service"
  tap_is "the timer runs at 07:00 and catches up" \
    "$(grep -q 'OnCalendar=\*-\*-\* 07:00:00' "$fx/cfg/systemd/user/$UNIT.timer" \
       && grep -q 'Persistent=true' "$fx/cfg/systemd/user/$UNIT.timer" && echo y)" y
  tap_run "the service runs this script's run" \
    grep -qx "ExecStart=$HERE/linkedin_digest.sh run" "$fx/cfg/systemd/user/$UNIT.service"
  tap_run "install enables the timer" grep -qx -- "--user enable --now $UNIT.timer" "$fx/systemctl.log"
  tap_end
}

case "${1:-}" in
  run) run_digest ;;
  install) install_units ;;
  status)
    systemctl --user list-timers "$UNIT.timer" --no-pager
    cat "$ROOT/.claude/linkedin_digest.last" 2>/dev/null || echo "never sent" ;;
  --self-test) self_test ;;
  *) echo "Usage: $0 run | install | status | --self-test" >&2; exit 2 ;;
esac
