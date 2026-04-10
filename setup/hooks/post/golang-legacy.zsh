#!/usr/bin/env zsh
setopt nullglob

source "$SCRIPT_DIR/setup/scripts/install-golang.zsh"
SHELL_RC=$(get_shell_rc)

if ! grep -q 'GOROOT' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Go paths (Legacy)' >>"$SHELL_RC"
  echo 'export GOROOT="$HOME/.go"' >>"$SHELL_RC"
  echo 'export GOPATH="$HOME/go"' >>"$SHELL_RC"
  echo 'export PATH="$GOROOT/bin:$GOPATH/bin:$PATH"' >>"$SHELL_RC"
fi
