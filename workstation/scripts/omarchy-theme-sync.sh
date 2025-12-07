#!/bin/bash

# Omarchy Theme Sync for Ghostty
# Syncs Ghostty config with current Omarchy theme
#
# Usage:
#   omarchy-theme-sync.sh           # Sync Ghostty with current Omarchy theme
#
# Run this after switching Omarchy themes with 'omarchy theme'

GHOSTTY_CONFIG="$HOME/.config/ghostty/config"
OMARCHY_THEME_DIR="$HOME/.config/omarchy/current/theme"
GHOSTTY_THEME="$OMARCHY_THEME_DIR/ghostty.conf"

# Check if Omarchy theme exists
if [ ! -d "$OMARCHY_THEME_DIR" ]; then
  echo "Error: Omarchy theme directory not found at $OMARCHY_THEME_DIR"
  exit 1
fi

# Check if Ghostty theme exists in current Omarchy theme
if [ ! -f "$GHOSTTY_THEME" ]; then
  echo "Warning: No ghostty.conf found in current Omarchy theme"
  echo "Theme path: $OMARCHY_THEME_DIR"
  exit 1
fi

# Create base config if it doesn't exist
if [ ! -f "$GHOSTTY_CONFIG.base" ]; then
  # Extract non-color settings from current config
  if [ -f "$GHOSTTY_CONFIG" ]; then
    grep -v -E "^(background|foreground|cursor|selection|palette)" "$GHOSTTY_CONFIG" > "$GHOSTTY_CONFIG.base"
  else
    # Create default base config
    cat > "$GHOSTTY_CONFIG.base" << 'EOF'
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

# Performance
vsync = true

EOF
  fi
fi

# Rebuild Ghostty config with theme colors
echo "Syncing Ghostty with Omarchy theme..."
cat "$GHOSTTY_CONFIG.base" > "$GHOSTTY_CONFIG"
echo "" >> "$GHOSTTY_CONFIG"
echo "# Theme: $(basename $(readlink -f $OMARCHY_THEME_DIR 2>/dev/null || echo $OMARCHY_THEME_DIR))" >> "$GHOSTTY_CONFIG"
cat "$GHOSTTY_THEME" >> "$GHOSTTY_CONFIG"

echo "Ghostty config updated with current Omarchy theme"
echo "Restart Ghostty or open a new window to see changes"
