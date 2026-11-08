#!/usr/bin/env bash
# digest.sh — run a digest skill, /<name>:digest, unattended once a day, and
# say when a run did not send.
#
# `install <name>…` enables the systemd user timer digest@<name>.timer, at
# 07:00 local time with Persistent=true, so a morning the machine missed runs
# once it is next active. Each run starts `claude -p /<name>:digest` in this
# repo with the tools the skill's allowed-tools names. A skill that names a
# playwright tool also gets the playwright server ~/.claude.json configures,
# plus --headless, since a timer has no display; any other skill runs with no
# MCP server. The service's PATH holds the directory of that server's command
# (npx) as `install` finds it, since a timer reads no shell profile: a new
# node version is reinstalled. When the project secret claude-oauth-token
# exists, mode 600, the run authenticates with it as CLAUDE_CODE_OAUTH_TOKEN,
# the one-year token `claude setup-token` prints.
#
# A run is sent when digest_mail.py rewrote .claude/digest/<name>.last during
# it; any other end (no server command, an expired login, a refused password,
# a failed claude) opens one journal note naming the run's last
# "Not sent: <reason>" line, or its last line when it printed none. The run
# appends to .claude/digest/<name>.log.
#
# Usage:
#   digest.sh run <name>          # what digest@<name>.service runs
#   digest.sh install <name>…     # write the units and enable each name's timer
#   digest.sh status              # each timer's last and next run, and each last send
#   digest.sh --self-test
#
# CLAUDE_BIN, CLAUDE_JSON, JOURNAL, SECRET_FILE and XDG_CONFIG_HOME exist for
# the self-test.
set -uo pipefail
case "${1:-}" in -h|--help) awk 'NR > 1 && !/^#/ { exit } NR > 1 { sub(/^# ?/, ""); print }' "$0"; exit 0 ;; esac
HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
ROOT=$(cd "$HERE/../.." && pwd)
STATE="$ROOT/.claude/digest"

unit_dir() { echo "${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user"; }

server_command() {
  jq -er '.mcpServers.playwright.command' "${CLAUDE_JSON:-$HOME/.claude.json}"
}

# The skill's allowed-tools, one per line.
skill_tools() {
  awk '/^---$/ { n++; next } n == 1 && /^allowed-tools:/ { sub(/^allowed-tools: */, ""); print }' \
    "$ROOT/.claude/skills/$1:digest/SKILL.md" | tr ',' '\n' | sed 's/^ *//; s/ *$//' | grep -v '^$'
}

service_text() {
  cat <<EOF
[Unit]
Description=Mail the %i digest (digest.sh run %i)

[Service]
Type=oneshot
Environment=PATH=%h/.local/bin:$1:/usr/local/bin:/usr/bin:/bin
ExecStart=$HERE/digest.sh run %i
EOF
}

timer_text() {
  cat <<EOF
[Unit]
Description=Daily %i digest at 07:00, caught up after a missed morning

[Timer]
OnCalendar=*-*-* 07:00:00
Persistent=true

[Install]
WantedBy=timers.target
EOF
}

install_units() {
  local d cmd bin name; d=$(unit_dir)
  [[ $# -gt 0 ]] || { echo "install needs a digest name" >&2; return 2; }
  for name in "$@"; do
    [[ -f "$ROOT/.claude/skills/$name:digest/SKILL.md" ]] || { echo "no skill $name:digest" >&2; return 1; }
  done
  cmd=$(server_command) || { echo "no playwright server in ${CLAUDE_JSON:-$HOME/.claude.json}" >&2; return 1; }
  bin=$(command -v "$cmd") || { echo "the playwright server's command $cmd is not on PATH" >&2; return 1; }
  mkdir -p "$d" || return 1
  service_text "$(dirname "$bin")" > "$d/digest@.service" && timer_text > "$d/digest@.timer" || return 1
  systemctl --user daemon-reload || return 1
  for name in "$@"; do
    systemctl --user enable --now "digest@$name.timer" || return 1
    echo "installed digest@$name.timer"
  done
}

run_digest() {
  local name=$1 log="$STATE/$1.log" stamp="$STATE/$1.last"
  local cfg mark rc=0 last start cmd secret tools=()
  cfg=$(mktemp); mark=$(mktemp); trap 'rm -f "$cfg" "$mark"' RETURN
  cd "$ROOT" || return 1
  mkdir -p "$STATE" "$ROOT/digest/$name" || return 1
  start=$(( $([[ -e "$log" ]] && wc -l < "$log" || echo 0) + 1 ))
  echo "== $(date -u +%FT%TZ) run started" >> "$log"
  mapfile -t tools < <(skill_tools "$name" 2>> "$log")
  if [[ ${#tools[@]} -eq 0 ]]; then
    echo "Not sent: no skill $name:digest with allowed-tools" >> "$log"; rc=1
  elif ! printf '%s\n' "${tools[@]}" | grep -q '^mcp__playwright__'; then
    echo '{"mcpServers":{}}' > "$cfg"
  elif ! cmd=$(server_command 2>> "$log") || ! jq -e '.mcpServers.playwright | .args += ["--headless"] | {mcpServers: {playwright: .}}' \
      "${CLAUDE_JSON:-$HOME/.claude.json}" > "$cfg" 2>> "$log"; then
    echo "Not sent: no playwright server in ${CLAUDE_JSON:-$HOME/.claude.json}" >> "$log"; rc=1
  elif ! command -v "$cmd" > /dev/null; then
    echo "Not sent: the playwright server's command $cmd is not on PATH; rerun digest.sh install $name" >> "$log"; rc=1
  fi
  if [[ $rc -eq 0 ]]; then
    if secret=$("${SECRET_FILE:-$HOME/.claude/scripts/secret_file.sh}" claude-oauth-token 2> /dev/null) \
        && [[ $(stat -c %a "$secret") == 600 ]]; then
      CLAUDE_CODE_OAUTH_TOKEN=$(< "$secret"); export CLAUDE_CODE_OAUTH_TOKEN
    fi
    "${CLAUDE_BIN:-claude}" -p "/$name:digest" --mcp-config "$cfg" --strict-mcp-config \
      --allowedTools "${tools[@]}" >> "$log" 2>&1 || rc=$?
  fi
  if [[ -e "$stamp" && "$stamp" -nt "$mark" ]]; then
    echo "== sent: $(cat "$stamp")" >> "$log"; return 0
  fi
  last=$(tail -n +"$start" "$log" | grep '^Not sent: ' | tail -n 1)
  [[ -n "$last" ]] || last=$(tail -n +"$start" "$log" | grep -v '^[[:space:]]*$' | grep -v '^== ' | tail -n 1)
  echo "== not sent (claude exit $rc)" >> "$log"
  env -u CLAUDE_CODE_SESSION_ID "${JOURNAL:-$HOME/.claude/scripts/journal.sh}" \
    -t "chore: $name digest" \
    -b "The $(date +%F) $name digest was not sent (claude exit $rc)." \
    -n "$name digest not sent — $(date +%F): ${last:-no output}; see .claude/digest/$name.log" \
    >> "$log" 2>&1
  return 1
}

self_test() {
  # shellcheck source=/dev/null
  . ~/.claude/scripts/tap.sh
  local fx; fx=$(mktemp -d); trap 'rm -rf "$fx"' EXIT
  mkdir -p "$fx/bin" "$fx/repo/.claude/scripts" "$fx/repo/.claude/skills/web:digest" "$fx/repo/.claude/skills/rss:digest"
  cp "$HERE/digest.sh" "$fx/repo/.claude/scripts/"
  local me="$fx/repo/.claude/scripts/digest.sh"
  printf -- '---\nname: web:digest\nallowed-tools: mcp__playwright__browser_navigate, Read, Bash(.claude/scripts/digest_mail.py *)\n---\nbody\n' \
    > "$fx/repo/.claude/skills/web:digest/SKILL.md"
  printf -- '---\nname: rss:digest\nallowed-tools: Read, Write\n---\nbody\n' > "$fx/repo/.claude/skills/rss:digest/SKILL.md"
  server() { printf '{"mcpServers":{"playwright":{"command":"%s","args":["-y","@playwright/mcp","--isolated"]}}}' "$1" > "$fx/claude.json"; }
  server fake-npx
  # A stand-in claude: records its arguments, the MCP config and the token, then does $FAKE.
  cat > "$fx/bin/claude" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$FX/args"
echo "${CLAUDE_CODE_OAUTH_TOKEN:-none}" > "$FX/token"
while [[ $# -gt 0 ]]; do [[ "$1" == --mcp-config ]] && cp "$2" "$FX/mcp"; shift; done
case "$FAKE" in
  send) sleep 1; echo "2026-11-08 07:00 -0700 sent: Feed title" > ".claude/digest/$NAME.last" ;;
  wall) echo "Not sent: LinkedIn login wall; run playwright_login.sh"; echo "Nothing changed state."; exit 0 ;;
esac
EOF
  printf '#!/usr/bin/env bash\nprintf "%%s\\n" "$@" > "$FX/journal"\n' > "$fx/bin/journal"
  printf '#!/usr/bin/env bash\necho "$*" >> "$FX/systemctl.log"\n' > "$fx/bin/systemctl"
  printf '#!/usr/bin/env bash\n' > "$fx/bin/fake-npx"
  printf '#!/usr/bin/env bash\n[[ -f "$FX/secret" ]] && echo "$FX/secret"\n' > "$fx/bin/secret_file"
  chmod +x "$fx/bin/"*
  go() { FX=$fx FAKE=$1 NAME=$2 PATH="$fx/bin:$PATH" CLAUDE_BIN="$fx/bin/claude" CLAUDE_JSON="$fx/claude.json" \
         JOURNAL="$fx/bin/journal" SECRET_FILE="$fx/bin/secret_file" "$me" run "$2"; }

  go send web
  tap_is "a run that sent exits 0 and opens no note" "$?$( [[ -e $fx/journal ]] && echo note)" 0
  tap_is "a skill naming a playwright tool runs with that server headless" \
    "$(jq -c '.mcpServers.playwright.args' "$fx/mcp")" '["-y","@playwright/mcp","--isolated","--headless"]'
  tap_is "claude runs the skill with only the tools its allowed-tools names" \
    "$(sed -n '/^--allowedTools$/,$p' "$fx/args" | tail -n +2 | paste -sd '|')" \
    'mcp__playwright__browser_navigate|Read|Bash(.claude/scripts/digest_mail.py *)'
  tap_is "the log records the send" "$(tail -n 1 "$fx/repo/.claude/digest/web.log")" \
    "== sent: 2026-11-08 07:00 -0700 sent: Feed title"
  tap_is "with no token secret it runs on the saved login" "$(cat "$fx/token")" none

  go send rss
  tap_is "a skill naming no playwright tool runs with no MCP server" "$(jq -c . "$fx/mcp")" '{"mcpServers":{}}'

  echo tok > "$fx/secret"; chmod 644 "$fx/secret"
  go send rss
  tap_is "a token secret others can read is not used" "$(cat "$fx/token")" none
  chmod 600 "$fx/secret"
  go send rss
  tap_is "a mode-600 token secret is the run's CLAUDE_CODE_OAUTH_TOKEN" "$(cat "$fx/token")" tok

  go wall web
  tap_is "a run that did not send exits 1" "$?" 1
  tap_is "its note names the digest and the run's Not sent line, not its last line" \
    "$(grep -c 'web digest not sent — .*login wall' "$fx/journal")$(grep -c 'Nothing changed' "$fx/journal")" 10

  rm -f "$fx/journal" "$fx/args"; server no-such-command-here
  go send web
  tap_is "with the server's command off PATH it does not run claude, and notes it" \
    "$?$([[ -e $fx/args ]] && echo ran)$(grep -c 'no-such-command-here is not on PATH' "$fx/journal")" 11

  rm -f "$fx/journal" "$fx/args"
  go send nosuch
  tap_is "an unknown digest does not run claude, and notes it" \
    "$?$([[ -e $fx/args ]] && echo ran)$(grep -c 'no skill nosuch:digest' "$fx/journal")" 11

  local out ud="$fx/cfg/systemd/user"
  server fake-npx
  ROOT="$fx/repo"
  if out=$(FX=$fx PATH="$fx/bin:$PATH" CLAUDE_JSON="$fx/claude.json" XDG_CONFIG_HOME="$fx/cfg" install_units web rss 2>&1); then
    tap_ok "the units install"
  else tap_not_ok "the units install" "$out"; fi
  tap_run "the service's PATH holds the server command's directory" \
    grep -qx "Environment=PATH=%h/.local/bin:$fx/bin:/usr/local/bin:/usr/bin:/bin" "$ud/digest@.service"
  tap_is "the timer runs at 07:00 and catches up" \
    "$(grep -q 'OnCalendar=\*-\*-\* 07:00:00' "$ud/digest@.timer" && grep -q 'Persistent=true' "$ud/digest@.timer" && echo y)" y
  tap_run "the service runs this script's run for its instance" grep -qx "ExecStart=$HERE/digest.sh run %i" "$ud/digest@.service"
  tap_is "install enables each name's timer" \
    "$(grep -c -e '--user enable --now digest@web.timer' -e '--user enable --now digest@rss.timer' "$fx/systemctl.log")" 2
  tap_is "install refuses a name with no skill" \
    "$(FX=$fx PATH="$fx/bin:$PATH" CLAUDE_JSON="$fx/claude.json" XDG_CONFIG_HOME="$fx/cfg" install_units nosuch 2>&1)" "no skill nosuch:digest"
  tap_end
}

case "${1:-}" in
  run) [[ -n "${2:-}" ]] || { echo "Usage: $0 run <name>" >&2; exit 2; }; run_digest "$2" ;;
  install) shift; install_units "$@" ;;
  status)
    systemctl --user list-timers 'digest@*' --no-pager
    for f in "$STATE"/*.last; do [[ -e "$f" ]] && echo "$(basename "$f" .last): $(cat "$f")"; done ;;
  --self-test) self_test ;;
  *) echo "Usage: $0 run <name> | install <name>… | status | --self-test" >&2; exit 2 ;;
esac
