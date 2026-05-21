#!/usr/bin/env bash
# Languages post-install hook (ASDF-managed).

echo "==> Configuring ASDF Version Manager"
if ! command -v brew >/dev/null 2>&1; then
  echo "  -> Homebrew not available; cannot locate asdf. Skipping."
  return 0 2>/dev/null || exit 0
fi
ASDF_DIR="$(brew --prefix asdf 2>/dev/null)/libexec"

SHELL_RC=$(get_shell_rc)
if ! grep -q 'asdf.sh' "$SHELL_RC" 2>/dev/null; then
  echo '' >> "$SHELL_RC"
  echo '# ASDF Version Manager' >> "$SHELL_RC"
  echo '. $(brew --prefix asdf)/libexec/asdf.sh' >> "$SHELL_RC"
fi

_asdf_installer="$SCRIPT_DIR/setup/scripts/asdf-install.sh"
if [ -x "$_asdf_installer" ]; then
  "$_asdf_installer" "$YADR_DIR/.tool-versions"
else
  echo "Error: $_asdf_installer not found or not executable"
fi
