#!/usr/bin/env bash
# Common utilities for YADRLite setup scripts.
#
# Sourced by both setup.zsh (zsh) and setup.sh (bash). Definitions below
# use the (a b c) array syntax that is supported by bash 3.2+ AND zsh, so
# the same file works in both shells. It will NOT work under dash/POSIX
# sh — that's why setup.sh is now a bash script, not a sh script.

export YADR_DIR="${YADR_DIR:-$HOME/.yadrlite}"
export DOTFILES_OLD="backup"

YADR_FILES=('vim' 'vimrc' 'tmux.conf' 'bash_profile' 'bashrc' 'vimrc.after')
YADR_CONFIG=('kitty' 'nvim' 'ranger')
YADR_TMUX_PLUGINS=('https://github.com/tmux-plugins/tmux-resurrect.git' 'https://github.com/tmux-plugins/tmux-sensible')

# Space-separated mirrors for callers that prefer string iteration. Keeps
# pre-existing POSIX call sites in setup.sh working without arrays.
export YADR_FILES_STR='vim vimrc tmux.conf bash_profile bashrc vimrc.after'
export YADR_CONFIG_STR='kitty nvim ranger'
export YADR_TMUX_PLUGINS_STR='https://github.com/tmux-plugins/tmux-resurrect.git https://github.com/tmux-plugins/tmux-sensible'

sed_i() {
  if [ "$(uname)" = "Darwin" ]; then
    sed -i '' "$@"
  else
    sed -i "$@"
  fi
}

# Returns the path of the user's primary shell rc file.
#
# Priority:
#   1. $YADR_SHELL_RC env override (set by install.sh or by the user).
#   2. ~/.bashrc when the install was flagged headless ($YADR_DIR/.headless)
#      or when zsh isn't on PATH.
#   3. ~/.zshrc otherwise (workstation default).
get_shell_rc() {
  if [ -n "${YADR_SHELL_RC:-}" ]; then
    echo "$YADR_SHELL_RC"
    return
  fi
  if [ -f "${YADR_DIR:-$HOME/.yadrlite}/.headless" ] || ! command -v zsh >/dev/null 2>&1; then
    echo "$HOME/.bashrc"
  else
    echo "$HOME/.zshrc"
  fi
}

# Returns 0 when this install was flagged headless (server / no-root box).
# Hooks use it to skip anything that needs sudo or a workstation-only tool.
is_headless() {
  [ -f "${YADR_DIR:-$HOME/.yadrlite}/.headless" ]
}

# Installs or updates tmux-revive (https://github.com/odysseyalive/tmux-revive):
# saves tmux sessions on detach, every minute and before shutdown, and restores
# them (Claude Code conversations included) on the next start. Its installer is
# root-free and idempotent: it unpacks into ~/.local/share/tmux-revive, links
# ~/.local/bin/tmux-revive, and adds a marked block to the tmux.conf and shell
# rc files in use. Rerunning it updates in place and re-adds the tmux.conf
# block, which the update path needs after regenerating $YADR_DIR/tmux.conf.
# Skipped without tmux so the installer never reaches for sudo to install it.
TMUX_REVIVE_INSTALLER="https://raw.githubusercontent.com/odysseyalive/tmux-revive/main/install.sh"
TMUX_REVIVE_APP="${XDG_DATA_HOME:-$HOME/.local/share}/tmux-revive/app"
install_tmux_revive() {
  if ! command -v tmux >/dev/null 2>&1; then
    echo "  -> Skipping tmux-revive install (tmux not found)"
    return 0
  fi
  if ! command -v curl >/dev/null 2>&1; then
    echo "  -> Skipping tmux-revive install (curl not found)"
    return 0
  fi
  echo "==> Installing tmux-revive"
  _revive_sh="$(mktemp "${TMPDIR:-/tmp}/yadrlite-tmux-revive.XXXXXX")" || return 0
  if curl -fsSL --max-time 120 "$TMUX_REVIVE_INSTALLER" -o "$_revive_sh" && sh "$_revive_sh"; then
    echo "  -> tmux-revive installed"
  else
    echo "  !! tmux-revive install failed"
  fi
  rm -f "$_revive_sh"
}
