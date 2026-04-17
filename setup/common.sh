#!/usr/bin/env zsh

# Common utilities for yadrlite setup scripts
export YADR_DIR=~/.yadrlite
export DOTFILES_OLD=backup

typeset -a YADR_FILES=('vim' 'vimrc' 'tmux.conf' 'bash_profile' 'bashrc' 'vimrc.after')
typeset -a YADR_CONFIG=('kitty' 'nvim' 'ranger')
typeset -a YADR_TMUX_PLUGINS=('https://github.com/tmux-plugins/tmux-resurrect.git' 'https://github.com/tmux-plugins/tmux-sensible')

sed_i() {
  if [[ "$(uname)" == "Darwin" ]]; then
    sed -i '' "$@"
  else
    sed -i "$@"
  fi
}

get_shell_rc() {
  echo "$HOME/.zshrc"
}
