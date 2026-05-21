#!/usr/bin/env bash
# Install/configure languages via ASDF using a .tool-versions file.

TOOL_VERSIONS_FILE="${1:-$HOME/.yadrlite/.tool-versions}"

if [ ! -s "$TOOL_VERSIONS_FILE" ]; then
  echo "  -> No .tool-versions file found or empty at $TOOL_VERSIONS_FILE"
  exit 0
fi

if ! command -v brew >/dev/null 2>&1; then
  echo "Error: Homebrew not available; cannot locate asdf" >&2
  exit 1
fi

ASDF_DIR="$(brew --prefix asdf 2>/dev/null)/libexec"
if [ -f "$ASDF_DIR/asdf.sh" ]; then
  # shellcheck disable=SC1090
  . "$ASDF_DIR/asdf.sh"
else
  echo "Error: ASDF not found at $ASDF_DIR" >&2
  exit 1
fi

echo "==> Setting up languages via ASDF"

sort -u "$TOOL_VERSIONS_FILE" -o "$TOOL_VERSIONS_FILE"

while read -r lang ver; do
  [ -z "$lang" ] && continue

  echo "  -> Configuring $lang ($ver)"
  asdf plugin add "$lang" 2>/dev/null || true

  # Ensure gnupg is installed for nodejs plugin signature verification on linux.
  if [ "$lang" = "nodejs" ] && [ "$(uname)" = "Linux" ]; then
    if ! command -v gpg >/dev/null 2>&1; then
      brew install gnupg
    fi
  fi

  asdf install "$lang" "$ver"
  asdf global "$lang" "$ver"
done < "$TOOL_VERSIONS_FILE"
