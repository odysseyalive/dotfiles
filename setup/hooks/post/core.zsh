#!/usr/bin/env zsh

SHELL_RC=$(get_shell_rc)

# Install pnpm for secure package management
if ! command -v pnpm &>/dev/null; then
  npm install -g pnpm
fi

[ -s "$SHELL_RC" ] && source "$SHELL_RC"
pnpm setup
pnpm install -g grunt-cli gulp gulp-cli csslint typescript typescript-language-server intelephense yaml-lint eslint-plugin-toml eslint-plugin-markdown golangci-lint vscode-css-languageserver-bin js-beautify unified-language-server eslint emmet-ls babel-eslint typescript-lsp coffeescript coffeelint neovim

# Go lang installs (now managed by brew, but we still need paths)
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

if ! grep -q 'export GOPATH="$HOME/go"' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Go paths' >>"$SHELL_RC"
  echo 'export GOPATH="$HOME/go"' >>"$SHELL_RC"
  echo 'export PATH="$GOPATH/bin:$PATH"' >>"$SHELL_RC"
fi

if ! grep -q 'export PATH="$HOME/.local/bin' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Local binaries' >>"$SHELL_RC"
  echo 'export PATH="$HOME/.local/bin:$PATH"' >>"$SHELL_RC"
fi

echo "# # Installing Nerd Font"
mkdir -p ~/.local/share/fonts
cp "$YADR_DIR/workstation/fonts/"*.ttf ~/.local/share/fonts/
if command -v fc-cache &>/dev/null; then
  fc-cache -fv ~/.local/share/fonts
fi

echo "# # Installing config files (kitty, nvim, ranger)"
mkdir -p ~/.config
mkdir -p "$YADR_DIR/backup/config" 2>/dev/null

for cfile in $YADR_CONFIG; do
  mv ~/.config/$cfile "$YADR_DIR/backup/config/$cfile" 2>/dev/null
  cp -r "$YADR_DIR/workstation/$cfile" ~/.config/$cfile
done
find ~/.config -type d -exec chmod 0755 {} \;
find ~/.config -type f -exec chmod 0644 {} \;
find ~/.config -name "*.sh" -execdir chmod u+x {} +

cp "$YADR_DIR/workstation/starship/starship.toml" ~/.config/starship.toml

if ! grep -q 'starship init zsh' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Starship prompt' >>"$SHELL_RC"
  echo 'eval "$(starship init zsh)"' >>"$SHELL_RC"
fi
