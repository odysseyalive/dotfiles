#!/usr/bin/env zsh

# Workstation Management Orchestrator
source "$(dirname "$0")/setup/common.sh"

action="${1:-setup}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%x}}")" && pwd)"

# Legacy compat: run specific files if invoked directly (e.g. tools.sh, update.sh)
if [[ -x "$SCRIPT_DIR/setup/${action}.sh" && "$action" != "setup" ]]; then
  "$SCRIPT_DIR/setup/${action}.sh"
  exit $?
fi

# Argument parsing for scalable setup
OS_OVERRIDE=""
typeset -a FEATURES=()

for arg in "$@"; do
  case $arg in
    --macos) OS_OVERRIDE="Darwin" ;;
    --linux) OS_OVERRIDE="Linux" ;;
    --with-*) FEATURES+=("${arg#--with-}") ;;
    setup|help) ;;
    *)
      # Alias backward compatibility for `setup tools`, `setup macos`, `setup keyboard`
      if [[ "$arg" != -* ]]; then
        if [[ "$arg" == "macos" ]]; then OS_OVERRIDE="Darwin"; fi
        if [[ "$arg" == "omarchy" ]]; then OS_OVERRIDE="Linux"; FEATURES+=("omarchy"); fi
        if [[ "$arg" == "linux" ]]; then OS_OVERRIDE="Linux"; fi
        if [[ "$arg" == "tools" ]]; then FEATURES+=("tools"); fi
        if [[ "$arg" == "keyboard" ]]; then FEATURES+=("keyboard"); fi
        if [[ "$arg" == "gnu" || "$arg" == "linuxify" ]]; then FEATURES+=("gnu"); fi
      else
        echo "Unknown argument: $arg"
      fi
      ;;
  esac
done

if [[ "$action" == "help" ]]; then
  echo "YADRLite Setup Management"
  echo "Usage: ./setup.sh [--macos | --linux] [--with-<feature>...]"
  echo ""
  echo "Features dynamically load from 'brewfiles/<feature>.Brewfile' or 'setup/hooks/pre|post/<feature>.zsh'"
  echo "Example: ./setup.sh --macos --with-gnu --with-keyboard"
  echo ""
  echo "Legacy Actions:"
  echo "  tools     - Alias for standard setup + node tools"
  echo "  macos     - Alias for --macos --with-macos"
  echo "  omarchy   - Alias for --linux --with-omarchy"
  echo "  keyboard  - Alias for --with-keyboard"
  echo "  update    - Update plugins and configurations"
  echo "  remove    - Uninstall YADRLite and restore backups"
  exit 0
fi

OS="${OS_OVERRIDE:-$(uname -s)}"
OS_LOWER="${(L)OS}"
if [[ "$OS_LOWER" == "darwin" ]]; then OS_LOWER="macos"; fi

# Apply implicit features from legacy args
if [[ "$action" == "macos" && ${FEATURES[(ie)macos]} -gt ${#FEATURES} ]]; then
  FEATURES+=("macos")
fi
if [[ "$action" == "tools" && ${FEATURES[(ie)tools]} -gt ${#FEATURES} ]]; then
  FEATURES+=("tools")
fi

echo "==> Starting YADRLite Setup (OS: $OS_LOWER)"

run_hook() {
  local hook_path="$1"
  if [[ -f "$hook_path" ]]; then
    echo "==> Running hook: $(basename "$hook_path")"
    source "$hook_path"
  fi
}

run_brewfile() {
  local brewfile_path="$1"
  if [[ -f "$brewfile_path" ]]; then
    echo "==> Installing packages from $(basename "$brewfile_path")"
    brew bundle --file="$brewfile_path" --no-lock
  fi
}

# 1. Core global setup
run_hook "$SCRIPT_DIR/setup/hooks/pre/core.zsh"
run_brewfile "$SCRIPT_DIR/brewfiles/global.Brewfile"

# 2. OS-specific setup
run_hook "$SCRIPT_DIR/setup/hooks/pre/${OS_LOWER}.zsh"
run_brewfile "$SCRIPT_DIR/brewfiles/${OS_LOWER}.Brewfile"

# 3. Feature modules
for feature in $FEATURES; do
  if [[ "$feature" != "$OS_LOWER" ]]; then
    run_hook "$SCRIPT_DIR/setup/hooks/pre/${feature}.zsh"
    run_brewfile "$SCRIPT_DIR/brewfiles/${feature}.Brewfile"
    run_hook "$SCRIPT_DIR/setup/hooks/post/${feature}.zsh"
  fi
done

# 4. Finalize
run_hook "$SCRIPT_DIR/setup/hooks/post/${OS_LOWER}.zsh"
run_hook "$SCRIPT_DIR/setup/hooks/post/core.zsh"

echo "==> Setup complete!"
