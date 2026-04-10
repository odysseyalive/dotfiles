#!/usr/bin/env zsh
source "$(dirname "$0")/common.sh"

SHELL_RC=$(get_shell_rc)

# Ensure Brew environment is loaded
if [ -f "/opt/homebrew/bin/brew" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -f "/usr/local/bin/brew" ]; then
  eval "$(/usr/local/bin/brew shellenv)"
elif [ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

echo "# # Installing system packages via Homebrew"
brew install ripgrep fd starship

curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash
sleep 5
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
nvm install v22.20.0
sleep 5
nvm alias default 22.20.0
nvm use 22.20.0

# Install pnpm for secure package management
npm install -g pnpm
[ -s "$SHELL_RC" ] && source "$SHELL_RC"
pnpm setup
pnpm install -g grunt-cli gulp gulp-cli csslint typescript typescript-language-server intelephense yaml-lint eslint-plugin-toml eslint-plugin-markdown golangci-lint vscode-css-languageserver-bin js-beautify unified-language-server eslint emmet-ls babel-eslint typescript-lsp coffeescript coffeelint neovim

# go lang
curl -sSL https://git.io/g-install | bash -s -- -y
sleep 5
export GOROOT="$HOME/.go"
export PATH="$GOROOT/bin:$PATH"
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

# Source the shell rc to ensure g is available
[ -s "$SHELL_RC" ] && source "$SHELL_RC"
g install latest
go install github.com/junegunn/fzf@latest
go install github.com/jesseduffield/lazygit@latest
go install github.com/zricethezav/gitleaks/v8@latest
go install github.com/charmbracelet/glow@latest

echo "# # Installing Nerd Font"
mkdir -p ~/.local/share/fonts
cp "$YADR_DIR/workstation/fonts/"*.ttf ~/.local/share/fonts/
if command -v fc-cache &>/dev/null; then
  fc-cache -fv ~/.local/share/fonts
fi

echo "# # Installing config files (kitty, nvim, ranger)"
echo "# # # # # # # # # # # # # # # # # # # # # # # # #"
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
