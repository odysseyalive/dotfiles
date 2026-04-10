#!/usr/bin/env zsh
setopt nullglob

echo "==> Configuring ASDF Version Manager"
ASDF_DIR="$(brew --prefix asdf 2>/dev/null)/libexec"

# Add asdf to shell rc if not present
SHELL_RC=$(get_shell_rc)
if ! grep -q 'asdf.sh' "$SHELL_RC" 2>/dev/null; then
  echo '' >> "$SHELL_RC"
  echo '# ASDF Version Manager' >> "$SHELL_RC"
  echo '. $(brew --prefix asdf)/libexec/asdf.sh' >> "$SHELL_RC"
fi

if [[ -x "$SCRIPT_DIR/setup/scripts/asdf-install.zsh" ]]; then
  "$SCRIPT_DIR/setup/scripts/asdf-install.zsh" "$YADR_DIR/.tool-versions"
else
  echo "Error: setup/scripts/asdf-install.zsh not found or not executable"
fi
