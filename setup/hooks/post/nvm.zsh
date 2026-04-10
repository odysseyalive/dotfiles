#!/usr/bin/env zsh
setopt nullglob

source "$SCRIPT_DIR/setup/scripts/install-nvm.zsh"
SHELL_RC=$(get_shell_rc)

if ! grep -q 'NVM_DIR' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# NVM Version Manager' >>"$SHELL_RC"
  echo 'export NVM_DIR="$HOME/.nvm"' >>"$SHELL_RC"
  echo '[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm' >>"$SHELL_RC"
  echo '[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion' >>"$SHELL_RC"
fi
