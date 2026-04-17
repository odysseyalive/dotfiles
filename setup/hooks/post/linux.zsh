#!/usr/bin/env zsh
setopt nullglob

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
INNER_EOF
