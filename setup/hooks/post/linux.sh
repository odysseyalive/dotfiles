#!/usr/bin/env bash
# Linux post-install hook. Targets workstation Linux (Omarchy etc.); the
# btrfs/font/Ghostty steps are skipped silently on headless servers without
# the required tools.

if command -v sudo >/dev/null 2>&1 && command -v btrfs >/dev/null 2>&1; then
  echo "# # Disabling btrfs quota"
  sudo btrfs quota disable / 2>/dev/null || true
fi

echo "# # Installing Fira Code Nerd Font"
mkdir -p ~/.local/share/fonts
_font_dir="$YADR_DIR/workstation/fonts"
if [ -d "$_font_dir" ]; then
  for _f in "$_font_dir"/*.ttf; do
    [ -e "$_f" ] || continue
    cp "$_f" ~/.local/share/fonts/
  done
fi

if command -v fc-cache >/dev/null 2>&1; then
  echo "# # Installing fontconfig for ligature support"
  mkdir -p ~/.config/fontconfig
  if [ -f "$YADR_DIR/workstation/fontconfig/fonts.conf" ]; then
    cp "$YADR_DIR/workstation/fontconfig/fonts.conf" ~/.config/fontconfig/
  fi
  fc-cache -fv
fi

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
