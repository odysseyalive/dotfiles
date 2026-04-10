#!/usr/bin/env zsh
setopt nullglob

echo "==> Configuring ASDF Version Manager"
ASDF_DIR="$(brew --prefix asdf)/libexec"

# Add asdf to shell rc if not present
SHELL_RC=$(get_shell_rc)
if ! grep -q 'asdf.sh' "$SHELL_RC" 2>/dev/null; then
  echo '' >> "$SHELL_RC"
  echo '# ASDF Version Manager' >> "$SHELL_RC"
  echo '. $(brew --prefix asdf)/libexec/asdf.sh' >> "$SHELL_RC"
fi

# Source ASDF for this script's execution context
if [ -f "$ASDF_DIR/asdf.sh" ]; then
  source "$ASDF_DIR/asdf.sh"
else
  echo "Error: ASDF not found at $ASDF_DIR"
  return 1
fi

typeset -a target_langs=()
for req in "${YADR_ASDF_LANGS[@]}"; do
  if [[ "$req" == "all" ]]; then
    target_langs+=("node" "python" "ruby" "golang")
  else
    target_langs+=("$req")
  fi
done

# Deduplicate requests
target_langs=("${(@u)target_langs}")

for lang_req in "${target_langs[@]}"; do
  # Parse format: lang-version or just lang
  if [[ "$lang_req" =~ ^([a-zA-Z0-9_]+)-(.+)$ ]]; then
    lang="${match[1]}"
    ver="${match[2]}"
  else
    lang="$lang_req"
    ver="latest"
  fi

  if [[ "$lang" == "go" ]]; then
    lang="golang"
  fi

  echo "  -> Setting up $lang ($ver)"
  asdf plugin add "$lang" 2>/dev/null || true
  
  # Special case for nodejs dependencies required by asdf node plugin
  if [[ "$lang" == "node" || "$lang" == "nodejs" ]]; then
    lang="nodejs"
    if [[ "$(uname)" == "Linux" ]]; then
       # The asdf nodejs plugin might need gpg dependencies on linux, but brew usually handles it.
    fi
    asdf plugin add "$lang" 2>/dev/null || true
  fi

  asdf install "$lang" "$ver"
  asdf global "$lang" "$ver"
done
