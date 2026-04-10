#!/usr/bin/env zsh
setopt nullglob

echo "==> [Migration v2.0.0] Pre-migration checks..."

mkdir -p "$YADR_DIR/cache"

# If we are using ASDF, check for legacy language managers and clean them up
if [[ "$USE_ASDF" == "1" ]]; then
  if [[ -d "$HOME/.nvm" ]]; then
    echo "    -> Found legacy NVM. Migrating Node.js to ASDF..."
    if [[ -s "$HOME/.nvm/nvm.sh" ]]; then
      # Source it and get current version
      source "$HOME/.nvm/nvm.sh"
      local nvm_ver=$(nvm current 2>/dev/null | sed 's/^v//')
      if [[ "$nvm_ver" != "none" && "$nvm_ver" != "system" && -n "$nvm_ver" ]]; then
        echo "    -> Node.js version $nvm_ver captured. Adding to .tool-versions."
        echo "nodejs $nvm_ver" >> "$YADR_DIR/.tool-versions"
        echo "{\"nodejs\": \"$nvm_ver\"}" > "$YADR_DIR/cache/migrated_langs.json"
      fi
    fi
    echo "    -> Archiving ~/.nvm to ~/.nvm.bak"
    mv "$HOME/.nvm" "$HOME/.nvm.bak" 2>/dev/null
  fi

  if [[ -d "$HOME/.go" || -f "$HOME/go/bin/g" ]]; then
    echo "    -> Found legacy 'g' Go installer. Migrating Golang to ASDF..."
    echo "golang latest" >> "$YADR_DIR/.tool-versions"
    echo "    -> Archiving ~/.go to ~/.go.bak"
    mv "$HOME/.go" "$HOME/.go.bak" 2>/dev/null
    mv "$HOME/go/bin/g" "$HOME/go/bin/g.bak" 2>/dev/null
  fi
fi
