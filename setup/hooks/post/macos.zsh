#!/usr/bin/env zsh
setopt nullglob

echo "# # Configuring Ghostty"
mkdir -p ~/.config/ghostty
cp "$YADR_DIR/workstation/macos/ghostty/config" ~/.config/ghostty/config

echo "# # Setting Ghostty as default terminal"
# duti is installed via macos.Brewfile
duti -s com.mitchellh.ghostty public.unix-executable all 2>/dev/null || true
duti -s com.mitchellh.ghostty public.shell-script all 2>/dev/null || true
duti -s com.mitchellh.ghostty .sh all 2>/dev/null || true
duti -s com.mitchellh.ghostty .bash all 2>/dev/null || true
duti -s com.mitchellh.ghostty .zsh all 2>/dev/null || true
echo "  Ghostty set as default for shell scripts and executables"

echo "# # Configuring AeroSpace"
mkdir -p ~/.config/aerospace
cp "$YADR_DIR/workstation/macos/aerospace/aerospace.toml" ~/.config/aerospace/aerospace.toml

echo "# # Configuring Sketchybar"
mkdir -p ~/.config/sketchybar/plugins
cp "$YADR_DIR/workstation/macos/sketchybar/sketchybarrc" ~/.config/sketchybar/sketchybarrc
cp "$YADR_DIR/workstation/macos/sketchybar/plugins/"*.sh ~/.config/sketchybar/plugins/
chmod +x ~/.config/sketchybar/sketchybarrc
chmod +x ~/.config/sketchybar/plugins/*.sh

echo "# # Configuring JankyBorders"
mkdir -p ~/.config/borders
cp "$YADR_DIR/workstation/macos/borders/bordersrc" ~/.config/borders/bordersrc
chmod +x ~/.config/borders/bordersrc

echo "# # Setting up SeaShells theme system"
mkdir -p ~/.config/themes
mkdir -p ~/.local/bin

chmod +x "$YADR_DIR/workstation/macos/scripts/theme-switch.sh"
ln -sf "$YADR_DIR/workstation/macos/scripts/theme-switch.sh" ~/.local/bin/theme-switch

bash "$YADR_DIR/workstation/macos/scripts/theme-switch.sh" auto

echo "# # Installing automatic theme watcher"
mkdir -p ~/Library/LaunchAgents
cp "$YADR_DIR/workstation/macos/scripts/com.yadrlite.theme-watcher.plist" ~/Library/LaunchAgents/
launchctl unload ~/Library/LaunchAgents/com.yadrlite.theme-watcher.plist 2>/dev/null || true
launchctl load ~/Library/LaunchAgents/com.yadrlite.theme-watcher.plist
echo "  Theme system installed with automatic light/dark mode switching!"

echo "# # Configuring shell (zsh)"
ZSHRC="$HOME/.zshrc"
if ! grep -q "YADRLite macOS" "$ZSHRC" 2>/dev/null; then
  cat >>"$ZSHRC" <<'INNER_EOF'

# YADRLite macOS configuration
# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)" 2>/dev/null || eval "$(/usr/local/bin/brew shellenv)" 2>/dev/null

# pnpm
export PNPM_HOME="$HOME/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"

# Go
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

# Local bin (theme-switch, etc.)
export PATH="$HOME/.local/bin:$PATH"

# Theme switch aliases
alias dark="theme-switch dark"
alias light="theme-switch light"
alias auto-theme="theme-switch auto"
INNER_EOF
  echo "Added YADRLite configuration to ~/.zshrc"
fi

if ! grep -q 'zoxide init zsh' "$ZSHRC" 2>/dev/null; then
  echo '' >>"$ZSHRC"
  echo '# Zoxide (smart cd)' >>"$ZSHRC"
  echo 'eval "$(zoxide init zsh)"' >>"$ZSHRC"
fi

echo ""
echo "IMPORTANT: The following applications will request system permissions:"
echo "1. AeroSpace - Accessibility (required for window management)"
echo "2. Sketchybar - Accessibility (for menu bar integration)"
echo ""

if command -v aerospace &>/dev/null; then
  open -a AeroSpace
fi
if command -v sketchybar &>/dev/null; then
  brew services start sketchybar
fi
if command -v borders &>/dev/null; then
  brew services start borders
fi
