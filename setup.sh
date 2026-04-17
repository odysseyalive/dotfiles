#!/bin/sh

# YADRLite Workstation Management Orchestrator (POSIX sh)
#
# Companion to setup.zsh for servers without zsh installed. Argument
# parsing, help, --upgrade, version management, and .tool-versions
# generation all run natively under sh. Hook files under
# setup/hooks/*/*.zsh are still zsh; they are dispatched via `zsh -c`
# when zsh is available and skipped with a warning otherwise.

SCRIPT_DIR=$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)

# Inline the sh-compatible subset of setup/common.sh. We cannot `.` that
# file because it uses zsh-only constructs (typeset -a, etc.).
YADR_DIR="${YADR_DIR:-$HOME/.yadrlite}"
export YADR_DIR
YADR_TMUX_PLUGINS="https://github.com/tmux-plugins/tmux-resurrect.git https://github.com/tmux-plugins/tmux-sensible"

tolower() {
  printf '%s' "$1" | tr '[:upper:]' '[:lower:]'
}

action="${1:-setup}"

# Legacy compat: run specific files if invoked directly
if [ -x "$SCRIPT_DIR/setup/${action}.sh" ] && [ "$action" != "setup" ]; then
  exec "$SCRIPT_DIR/setup/${action}.sh"
fi

OS_OVERRIDE=""
FEATURES=""
YADR_ASDF_LANGS=""
USE_ASDF=1
MIGRATE=0
UPGRADE=0
HAS_MACOS_FLAG=0
HAS_LINUX_FLAG=0

for arg in "$@"; do
  case "$arg" in
    --macos)
      OS_OVERRIDE="Darwin"
      HAS_MACOS_FLAG=1
      ;;
    --linux)
      OS_OVERRIDE="Linux"
      HAS_LINUX_FLAG=1
      ;;
    --without-asdf) USE_ASDF=0 ;;
    --migrate) MIGRATE=1 ;;
    --upgrade) UPGRADE=1 ;;
    --with-langs)
      FEATURES="$FEATURES langs"
      YADR_ASDF_LANGS="$YADR_ASDF_LANGS all"
      ;;
    --with-lang-*)
      FEATURES="$FEATURES langs"
      YADR_ASDF_LANGS="$YADR_ASDF_LANGS ${arg#--with-lang-}"
      ;;
    --with-*) FEATURES="$FEATURES ${arg#--with-}" ;;
    setup|help) ;;
    -*)
      echo "Unknown argument: $arg"
      ;;
    *)
      case "$arg" in
        macos) OS_OVERRIDE="Darwin" ;;
        omarchy) OS_OVERRIDE="Linux"; FEATURES="$FEATURES omarchy" ;;
        linux) OS_OVERRIDE="Linux" ;;
        tools) FEATURES="$FEATURES tools" ;;
        keyboard) FEATURES="$FEATURES keyboard" ;;
        gnu|linuxify) FEATURES="$FEATURES gnu" ;;
        update|upgrade) UPGRADE=1 ;;
      esac
      ;;
  esac
done

if [ "$action" = "remove" ] || [ "$action" = "uninstall" ]; then
  shift
  exec "$SCRIPT_DIR/uninstall.sh" "$@"
fi

if [ "$HAS_MACOS_FLAG" -eq 1 ] && [ "$HAS_LINUX_FLAG" -eq 1 ]; then
  echo "Error: --macos and --linux are mutually exclusive override flags. You can only specify one."
  exit 1
fi

if [ "$action" = "help" ]; then
  cat <<'EOF'
YADRLite Setup Management
Usage: ./setup.sh [--macos | --linux] [--with-<feature>...] [--migrate] [--upgrade]

OS Selection:
  (Autodetects by default)
  --macos   Force setup as macOS
  --linux   Force setup as Linux

Features dynamically load from 'brewfiles/<feature>.Brewfile' or 'setup/hooks/pre|post/<feature>.zsh'
Example 1: ./setup.sh --macos --with-gnu --with-keyboard
Example 2: ./setup.sh --with-langs                # Installs node, python, ruby, golang via asdf
Example 3: ./setup.sh --with-lang-ruby-3.2.0      # Installs a specific language and version via asdf
Example 4: ./setup.sh --with-tools --without-asdf # Installs node via nvm and go via g-install instead

Legacy Actions:
  tools     - Alias for standard setup + node tools
  macos     - Alias for --macos --with-macos
  omarchy   - Alias for --linux --with-omarchy
  keyboard  - Alias for --with-keyboard
  update    - Alias for --upgrade
  remove    - Alias for uninstall.sh
EOF
  exit 0
fi

if [ "$UPGRADE" = "1" ]; then
  echo "==> Updating YADRLite repository..."
  cd "$YADR_DIR" || { echo "Error: YADR_DIR ($YADR_DIR) not found"; exit 1; }
  git pull --rebase

  # Re-generate the top-level tmux.conf that ~/.tmux.conf and
  # ~/.config/tmux/tmux.conf symlink to. install.sh seeds this with
  # `cat tmux/tmux.conf > tmux.conf`; without refreshing it, edits to
  # tmux/tmux.conf in the repo never reach existing installs.
  echo "==> Refreshing top-level tmux.conf..."
  cat "$YADR_DIR/tmux/tmux.conf" >"$YADR_DIR/tmux.conf"

  echo "==> Updating tmux plugins..."
  mkdir -p "$YADR_DIR/tmux/plugin"
  for tplug in $YADR_TMUX_PLUGINS; do
    plugname=$(basename "$tplug" .git)
    if [ ! -d "$YADR_DIR/tmux/plugin/$plugname" ]; then
      echo "  -> Installing $plugname..."
      git clone "$tplug" "$YADR_DIR/tmux/plugin/$plugname"
    fi
  done
  for pdir in "$YADR_DIR"/tmux/plugin/*/; do
    [ -d "$pdir" ] || continue
    [ -d "$pdir/.git" ] || continue
    echo "  -> Updating $(basename "$pdir")..."
    ( cd "$pdir" && git pull --rebase )
  done

  echo "==> Repository updated. Resuming setup..."
fi

CURRENT_VERSION="1.0.0"
if [ -f "$SCRIPT_DIR/VERSION" ]; then
  CURRENT_VERSION=$(cat "$SCRIPT_DIR/VERSION")
fi

INSTALLED_VERSION="1.0.0"
if [ -f "$YADR_DIR/.installed_version" ]; then
  INSTALLED_VERSION=$(cat "$YADR_DIR/.installed_version")
fi

version_gt() {
  test "$(printf '%s\n' "$@" | sort -V | head -n 1)" != "$1"
}

OS="${OS_OVERRIDE:-$(uname -s)}"
OS_LOWER=$(tolower "$OS")
[ "$OS_LOWER" = "darwin" ] && OS_LOWER="macos"

case " $FEATURES " in
  *" macos "*) ;;
  *) [ "$action" = "macos" ] && FEATURES="$FEATURES macos" ;;
esac
case " $FEATURES " in
  *" tools "*) ;;
  *) [ "$action" = "tools" ] && FEATURES="$FEATURES tools" ;;
esac

case " $FEATURES " in
  *" tools "*)
    if [ "$USE_ASDF" = "1" ]; then
      FEATURES="$FEATURES langs"
      YADR_ASDF_LANGS="$YADR_ASDF_LANGS nodejs golang"
    else
      FEATURES="$FEATURES nvm golang-legacy"
    fi
    ;;
esac

# Deduplicate and pull priority features (langs > golang-legacy > nvm)
# to the front so they run before 'tools'.
dedup_and_reorder() {
  _deduped=""
  for _tok in $FEATURES; do
    case " $_deduped " in
      *" $_tok "*) ;;
      *) _deduped="$_deduped $_tok" ;;
    esac
  done
  _ordered=""
  for _pri in langs golang-legacy nvm; do
    case " $_deduped " in
      *" $_pri "*)
        _ordered="$_ordered $_pri"
        _new=""
        for _tok in $_deduped; do
          [ "$_tok" = "$_pri" ] && continue
          _new="$_new $_tok"
        done
        _deduped="$_new"
        ;;
    esac
  done
  FEATURES=$(printf '%s %s' "$_ordered" "$_deduped" | sed 's/^ *//; s/ *$//; s/  */ /g')
}
dedup_and_reorder

export USE_ASDF

TOOL_VERSIONS="$YADR_DIR/.tool-versions"
touch "$TOOL_VERSIONS"

if [ "$USE_ASDF" = "1" ]; then
  for req in $YADR_ASDF_LANGS; do
    case "$req" in
      all)
        printf 'nodejs latest\npython latest\nruby latest\ngolang latest\n' >>"$TOOL_VERSIONS"
        ;;
      *-*)
        lang="${req%%-*}"
        ver="${req#*-}"
        [ "$lang" = "node" ] && lang="nodejs"
        [ "$lang" = "go" ] && lang="golang"
        echo "$lang $ver" >>"$TOOL_VERSIONS"
        ;;
      *)
        lang="$req"
        [ "$lang" = "node" ] && lang="nodejs"
        [ "$lang" = "go" ] && lang="golang"
        echo "$lang latest" >>"$TOOL_VERSIONS"
        ;;
    esac
  done
  if [ -s "$TOOL_VERSIONS" ]; then
    sort -u "$TOOL_VERSIONS" -o "$TOOL_VERSIONS"
  fi
fi

echo "==> Starting YADRLite Setup (OS: $OS_LOWER, Version: $CURRENT_VERSION)"

# Hook runner: prefers .sh sibling; otherwise dispatches .zsh through
# zsh with setup/common.sh pre-sourced. Skips with a warning when the
# hook is .zsh-only and zsh is not installed.
run_hook() {
  _hook_base="$1"
  if [ -f "${_hook_base}.sh" ]; then
    echo "==> Running hook: $(basename "${_hook_base}.sh")"
    # shellcheck disable=SC1090
    . "${_hook_base}.sh"
  elif [ -f "${_hook_base}.zsh" ]; then
    if command -v zsh >/dev/null 2>&1; then
      echo "==> Running hook: $(basename "${_hook_base}.zsh")"
      zsh -c ". \"$SCRIPT_DIR/setup/common.sh\" && . \"${_hook_base}.zsh\""
    else
      echo "  ! Skipping hook $(basename "${_hook_base}.zsh") (zsh not installed)"
    fi
  fi
}

run_brewfile() {
  _bf="$1"
  if [ -f "$_bf" ]; then
    echo "==> Installing packages from $(basename "$_bf")"
    brew bundle --file="$_bf" --no-lock
  fi
}

if [ "$MIGRATE" = "1" ] && [ "$INSTALLED_VERSION" != "$CURRENT_VERSION" ]; then
  echo "==> Migration requested: $INSTALLED_VERSION -> $CURRENT_VERSION"
  for mig_dir in "$SCRIPT_DIR/setup/migrations"/v*/; do
    [ -d "$mig_dir" ] || continue
    mig_name=$(basename "$mig_dir")
    mig_ver="${mig_name#v}"
    if version_gt "$mig_ver" "$INSTALLED_VERSION" \
      || { [ "$mig_ver" = "$CURRENT_VERSION" ] && [ "$INSTALLED_VERSION" = "1.0.0" ]; }; then
      run_hook "${mig_dir%/}/pre"
    fi
  done
fi

run_hook "$SCRIPT_DIR/setup/hooks/pre/core"
run_brewfile "$SCRIPT_DIR/brewfiles/global.Brewfile"

run_hook "$SCRIPT_DIR/setup/hooks/pre/${OS_LOWER}"
run_brewfile "$SCRIPT_DIR/brewfiles/${OS_LOWER}.Brewfile"

for feature in $FEATURES; do
  if [ "$feature" != "$OS_LOWER" ]; then
    run_hook "$SCRIPT_DIR/setup/hooks/pre/${feature}"
    run_brewfile "$SCRIPT_DIR/brewfiles/${feature}.Brewfile"
    run_hook "$SCRIPT_DIR/setup/hooks/post/${feature}"
  fi
done

run_hook "$SCRIPT_DIR/setup/hooks/post/${OS_LOWER}"
run_hook "$SCRIPT_DIR/setup/hooks/post/core"

if [ "$MIGRATE" = "1" ] && [ "$INSTALLED_VERSION" != "$CURRENT_VERSION" ]; then
  for mig_dir in "$SCRIPT_DIR/setup/migrations"/v*/; do
    [ -d "$mig_dir" ] || continue
    mig_name=$(basename "$mig_dir")
    mig_ver="${mig_name#v}"
    if version_gt "$mig_ver" "$INSTALLED_VERSION" \
      || { [ "$mig_ver" = "$CURRENT_VERSION" ] && [ "$INSTALLED_VERSION" = "1.0.0" ]; }; then
      run_hook "${mig_dir%/}/post"
    fi
  done
  echo "$CURRENT_VERSION" >"$YADR_DIR/.installed_version"
  echo "==> Migration complete!"
fi

echo "==> Setup complete!"
