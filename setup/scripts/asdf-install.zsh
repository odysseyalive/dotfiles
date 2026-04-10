#!/usr/bin/env zsh
setopt nullglob

TOOL_VERSIONS_FILE="${1:-$HOME/.yadrlite/.tool-versions}"

if [[ ! -s "$TOOL_VERSIONS_FILE" ]]; then
  echo "  -> No .tool-versions file found or empty at $TOOL_VERSIONS_FILE"
  exit 0
fi

# Ensure ASDF is loaded
ASDF_DIR="$(brew --prefix asdf 2>/dev/null)/libexec"
if [[ -f "$ASDF_DIR/asdf.sh" ]]; then
  source "$ASDF_DIR/asdf.sh"
else
  echo "Error: ASDF not found at $ASDF_DIR"
  exit 1
fi

echo "==> Setting up languages via ASDF"

# Sort and deduplicate the tool-versions file
sort -u "$TOOL_VERSIONS_FILE" -o "$TOOL_VERSIONS_FILE"

cat "$TOOL_VERSIONS_FILE" | while read -r lang ver; do
  [[ -z "$lang" ]] && continue
  
  echo "  -> Configuring $lang ($ver)"
  asdf plugin add "$lang" 2>/dev/null || true
  
  # Ensure gnupg is installed for nodejs plugin signature verification on linux
  if [[ "$lang" == "nodejs" && "$(uname)" == "Linux" ]]; then
    if ! command -v gpg &>/dev/null; then
      brew install gnupg
    fi
  fi
  
  asdf install "$lang" "$ver"
  asdf global "$lang" "$ver"
done
