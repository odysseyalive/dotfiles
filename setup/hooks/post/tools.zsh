#!/usr/bin/env zsh
setopt nullglob

SHELL_RC=$(get_shell_rc)

echo "==> Configuring Node.js via NVM"
export NVM_DIR="$HOME/.nvm"
mkdir -p "$NVM_DIR"

if [ -s "$(brew --prefix)/opt/nvm/nvm.sh" ]; then
  source "$(brew --prefix)/opt/nvm/nvm.sh"
elif [ -s "$NVM_DIR/nvm.sh" ]; then
  source "$NVM_DIR/nvm.sh"
fi

if command -v nvm &>/dev/null; then
  nvm install v22.20.0
  nvm alias default 22.20.0
  nvm use 22.20.0
else
  echo "Warning: NVM not found. Skipping Node.js installation."
fi

# Install pnpm for secure package management
if command -v npm &>/dev/null; then
  npm install -g pnpm
  [ -s "$SHELL_RC" ] && source "$SHELL_RC"
  pnpm setup
  pnpm install -g grunt-cli gulp gulp-cli csslint typescript typescript-language-server intelephense yaml-lint eslint-plugin-toml eslint-plugin-markdown golangci-lint vscode-css-languageserver-bin js-beautify unified-language-server eslint emmet-ls babel-eslint typescript-lsp coffeescript coffeelint neovim
fi

# Go lang installs (now managed by brew, but we still need paths)
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

if ! grep -q 'export GOPATH="$HOME/go"' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Go paths' >>"$SHELL_RC"
  echo 'export GOPATH="$HOME/go"' >>"$SHELL_RC"
  echo 'export PATH="$GOPATH/bin:$PATH"' >>"$SHELL_RC"
fi
