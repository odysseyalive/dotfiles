#!/usr/bin/env zsh

echo "# # Setting up GNU utilities (Linuxify) aliases"

ZSHRC="$HOME/.zshrc"

if [[ "$(uname)" == "Darwin" ]]; then
  if ! grep -q "gnubin" "$ZSHRC" 2>/dev/null; then
    cat >>"$ZSHRC" <<'INNER_EOF'

# GNU Coreutils/Linuxify
export PATH="$(brew --prefix)/opt/coreutils/libexec/gnubin:$PATH"
export PATH="$(brew --prefix)/opt/findutils/libexec/gnubin:$PATH"
export PATH="$(brew --prefix)/opt/gnu-sed/libexec/gnubin:$PATH"
export PATH="$(brew --prefix)/opt/gnu-tar/libexec/gnubin:$PATH"
export PATH="$(brew --prefix)/opt/gawk/libexec/gnubin:$PATH"
export PATH="$(brew --prefix)/opt/grep/libexec/gnubin:$PATH"
export MANPATH="$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH"
export MANPATH="$(brew --prefix)/opt/findutils/libexec/gnuman:$MANPATH"
export MANPATH="$(brew --prefix)/opt/gnu-sed/libexec/gnuman:$MANPATH"
export MANPATH="$(brew --prefix)/opt/gnu-tar/libexec/gnuman:$MANPATH"
export MANPATH="$(brew --prefix)/opt/gawk/libexec/gnuman:$MANPATH"
export MANPATH="$(brew --prefix)/opt/grep/libexec/gnuman:$MANPATH"

alias make="gmake"
INNER_EOF
    echo "Added GNU tools (linuxify) to PATH in ~/.zshrc"
  fi
fi
