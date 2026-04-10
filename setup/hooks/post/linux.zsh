#!/usr/bin/env zsh

echo "# # Disabling btrfs quota"
sudo btrfs quota disable / 2>/dev/null || true

echo "# # Installing Fira Code Nerd Font"
mkdir -p ~/.local/share/fonts
cp "$YADR_DIR/workstation/fonts/"*.ttf ~/.local/share/fonts/

echo "# # Installing fontconfig for ligature support"
mkdir -p ~/.config/fontconfig
cp "$YADR_DIR/workstation/fontconfig/fonts.conf" ~/.config/fontconfig/
fc-cache -fv

echo "# # Configuring Kitty for Omarchy theme switching"
mkdir -p ~/.config/kitty
cat >~/.config/kitty/kitty.conf <<'INNER_EOF'
# Omarchy theme integration - automatically updates when you switch themes
include ~/.config/omarchy/current/theme/kitty.conf

# Add your personal Kitty settings below this line
# (fonts, keybindings, window settings, etc.)

# Enable remote control for live theme switching (omarchy-theme-sync)
allow_remote_control yes
INNER_EOF

echo "# # Configuring Ghostty for Omarchy theme switching"
mkdir -p ~/.config/ghostty
cat >~/.config/ghostty/config <<'INNER_EOF'
# Ghostty Configuration for YADRLite + Omarchy
font-family = FiraCode Nerd Font
font-size = 10.5
term = xterm-256color
scrollback-limit = 10000
clipboard-read = allow
clipboard-write = allow
mouse-hide-while-typing = true

# Theme (synced from Omarchy)
# Run 'omarchy-theme-sync' after switching themes
INNER_EOF

if [ -f ~/.config/omarchy/current/theme/kitty.conf ]; then
  bash "$YADR_DIR/workstation/scripts/omarchy-theme-sync.sh"
fi

mkdir -p ~/.local/bin
chmod +x "$YADR_DIR/workstation/scripts/omarchy-theme-sync.sh"
ln -sf "$YADR_DIR/workstation/scripts/omarchy-theme-sync.sh" ~/.local/bin/omarchy-theme-sync

mkdir -p ~/.config/omarchy/hooks
ln -sf "$YADR_DIR/workstation/scripts/omarchy-theme-sync.sh" ~/.config/omarchy/hooks/theme-set
