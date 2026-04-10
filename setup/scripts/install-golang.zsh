#!/usr/bin/env zsh
setopt nullglob

echo "==> Installing Go Version Manager (g-install)"
curl -sSL https://git.io/g-install | bash -s -- -y
export GOROOT="$HOME/.go"
export GOPATH="$HOME/go"
export PATH="$GOROOT/bin:$GOPATH/bin:$PATH"

if command -v g >/dev/null 2>&1; then
  g install latest
else
  echo "Warning: g-install failed to place 'g' in PATH"
fi
