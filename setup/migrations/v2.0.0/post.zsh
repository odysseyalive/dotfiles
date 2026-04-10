#!/usr/bin/env zsh
setopt nullglob

echo "==> [Migration v2.0.0] Post-migration cleanup..."

SHELL_RC=$(get_shell_rc)

# Clean up legacy paths from shell rc if switching to ASDF
if [[ "$USE_ASDF" == "1" ]]; then
  echo "    -> Removing legacy NVM and Goroots from $SHELL_RC"
  sed_i '/NVM_DIR/d' "$SHELL_RC"
  sed_i '/nvm.sh/d' "$SHELL_RC"
  sed_i '/bash_completion/d' "$SHELL_RC"
  sed_i '/GOROOT/d' "$SHELL_RC"
  sed_i '/GOPATH/d' "$SHELL_RC"
fi
