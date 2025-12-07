#!/bin/bash

# Omarchy Theme Sync for Ghostty
# Syncs Ghostty theme with current Omarchy theme
#
# Usage:
#   omarchy-theme-sync           # Sync Ghostty with current Omarchy theme
#
# Run this after switching Omarchy themes with 'omarchy theme'

GHOSTTY_CONFIG="$HOME/.config/ghostty/config"
GHOSTTY_THEMES_DIR="$HOME/.config/ghostty/themes"
OMARCHY_THEME_DIR="$HOME/.config/omarchy/current/theme"
KITTY_THEME="$OMARCHY_THEME_DIR/kitty.conf"
GHOSTTY_THEME_CONF="$OMARCHY_THEME_DIR/ghostty.conf"

# Check if Omarchy theme exists
if [ ! -d "$OMARCHY_THEME_DIR" ]; then
  echo "Error: Omarchy theme directory not found at $OMARCHY_THEME_DIR"
  exit 1
fi

# Get theme name from Omarchy ghostty.conf or kitty.conf
THEME_NAME=""
if [ -f "$GHOSTTY_THEME_CONF" ]; then
  THEME_NAME=$(grep -oP 'theme\s*=\s*\K\S+' "$GHOSTTY_THEME_CONF" | head -1)
fi
if [ -z "$THEME_NAME" ] && [ -f "$KITTY_THEME" ]; then
  THEME_NAME=$(grep -oP '##\s*name:\s*\K.*' "$KITTY_THEME" | head -1)
fi
if [ -z "$THEME_NAME" ]; then
  THEME_NAME="Omarchy"
fi

# Check if kitty.conf exists (source of color definitions)
if [ ! -f "$KITTY_THEME" ]; then
  echo "Warning: No kitty.conf found in current Omarchy theme"
  echo "Theme path: $OMARCHY_THEME_DIR"
  exit 1
fi

# Create themes directory
mkdir -p "$GHOSTTY_THEMES_DIR"

# Extract colors from kitty.conf and create Ghostty theme
echo "Creating Ghostty theme: $THEME_NAME"

THEME_FILE="$GHOSTTY_THEMES_DIR/$THEME_NAME"

cat > "$THEME_FILE" << EOF
# $THEME_NAME Theme for Ghostty
# Auto-generated from Omarchy theme by omarchy-theme-sync

EOF

# Extract and convert colors from kitty.conf
{
  grep -E "^background\s+" "$KITTY_THEME" | sed 's/background\s\+#\?/background = /'
  grep -E "^foreground\s+" "$KITTY_THEME" | sed 's/foreground\s\+#\?/foreground = /'
  grep -E "^cursor\s+" "$KITTY_THEME" | sed 's/cursor\s\+#\?/cursor-color = /'
  grep -E "^cursor_text_color\s+" "$KITTY_THEME" | sed 's/cursor_text_color\s\+#\?/cursor-text = /'
  grep -E "^selection_background\s+" "$KITTY_THEME" | sed 's/selection_background\s\+#\?/selection-background = /'
  grep -E "^selection_foreground\s+" "$KITTY_THEME" | sed 's/selection_foreground\s\+#\?/selection-foreground = /'
  echo ""
  echo "# Normal colors"
  for i in {0..7}; do
    grep -E "^color$i\s+" "$KITTY_THEME" | sed "s/color$i\s\+/palette = $i=/"
  done
  echo ""
  echo "# Bright colors"
  for i in {8..15}; do
    grep -E "^color$i\s+" "$KITTY_THEME" | sed "s/color$i\s\+/palette = $i=/"
  done
} >> "$THEME_FILE"

# Update Ghostty config to use the theme
if [ -f "$GHOSTTY_CONFIG" ]; then
  # Update existing theme line or add it
  if grep -q "^theme\s*=" "$GHOSTTY_CONFIG"; then
    sed -i "s/^theme\s*=.*/theme = $THEME_NAME/" "$GHOSTTY_CONFIG"
  else
    echo "theme = $THEME_NAME" >> "$GHOSTTY_CONFIG"
  fi
else
  # Create basic config
  cat > "$GHOSTTY_CONFIG" << EOF
# Ghostty Configuration for YADRLite + Omarchy
# https://ghostty.org

# Font settings
font-family = FiraCode Nerd Font
font-size = 10.5

# Terminal settings
term = xterm-256color

# Scrollback
scrollback-limit = 10000

# Clipboard
clipboard-read = allow
clipboard-write = allow

# Mouse
mouse-hide-while-typing = true

# Theme (synced from Omarchy)
theme = $THEME_NAME
EOF
fi

echo "Ghostty theme '$THEME_NAME' created at $THEME_FILE"
echo "Ghostty config updated to use theme"
echo "Restart Ghostty or open a new window to see changes"
