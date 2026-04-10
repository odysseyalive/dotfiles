#!/usr/bin/env zsh
setopt nullglob

SHELL_RC=$(get_shell_rc)

# Ensure ASDF is loaded in this sub-shell session if it exists so npm works
ASDF_DIR="$(brew --prefix asdf)/libexec"
if [ -f "$ASDF_DIR/asdf.sh" ]; then
  source "$ASDF_DIR/asdf.sh"
fi

echo "==> Configuring Global Packages & Tooling"

# Install pnpm for secure package management
if command -v npm &>/dev/null; then
  if ! command -v pnpm &>/dev/null; then
    npm install -g pnpm
  fi

  [ -s "$SHELL_RC" ] && source "$SHELL_RC"
  pnpm setup
  pnpm install -g grunt-cli gulp gulp-cli csslint typescript typescript-language-server intelephense yaml-lint eslint-plugin-toml eslint-plugin-markdown golangci-lint vscode-css-languageserver-bin js-beautify unified-language-server eslint emmet-ls babel-eslint typescript-lsp coffeescript coffeelint neovim
else
  echo "  -> Skipping Node.js tooling setup (npm not found)"
fi

# Go lang installs (now managed by brew/asdf, but we still need paths)
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

if ! grep -q 'export GOPATH="$HOME/go"' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Go paths' >>"$SHELL_RC"
  echo 'export GOPATH="$HOME/go"' >>"$SHELL_RC"
  echo 'export PATH="$GOPATH/bin:$PATH"' >>"$SHELL_RC"
fi
