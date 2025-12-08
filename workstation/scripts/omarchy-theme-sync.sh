#!/bin/bash

# Omarchy Theme Sync for Ghostty, Starship, and Tmux
# Syncs Ghostty, Starship, and Tmux themes with current Omarchy theme
#
# Usage:
#   omarchy-theme-sync           # Sync themes with current Omarchy theme
#
# Run this after switching Omarchy themes with 'omarchy theme'

GHOSTTY_CONFIG="$HOME/.config/ghostty/config"
GHOSTTY_THEMES_DIR="$HOME/.config/ghostty/themes"
STARSHIP_CONFIG="$HOME/.config/starship.toml"
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

# Extract colors from kitty.conf
echo "Extracting colors from Omarchy theme: $THEME_NAME"

# Get key colors for Starship palette
COLOR_YELLOW=$(grep -E "^color3\s+" "$KITTY_THEME" | awk '{print $2}')
COLOR_CYAN=$(grep -E "^color6\s+" "$KITTY_THEME" | awk '{print $2}')
COLOR_MAGENTA=$(grep -E "^color5\s+" "$KITTY_THEME" | awk '{print $2}')
COLOR_GREEN=$(grep -E "^color2\s+" "$KITTY_THEME" | awk '{print $2}')
COLOR_BLUE=$(grep -E "^color4\s+" "$KITTY_THEME" | awk '{print $2}')

# Fallback to defaults if colors not found
COLOR_YELLOW=${COLOR_YELLOW:-#fba02f}
COLOR_CYAN=${COLOR_CYAN:-#50a3b5}
COLOR_MAGENTA=${COLOR_MAGENTA:-#68d3f0}
COLOR_GREEN=${COLOR_GREEN:-#027b9b}
COLOR_BLUE=${COLOR_BLUE:-#2d6870}

# ============================================
# Update Ghostty theme
# ============================================
mkdir -p "$GHOSTTY_THEMES_DIR"
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
  if grep -q "^theme\s*=" "$GHOSTTY_CONFIG"; then
    sed -i "s/^theme\s*=.*/theme = $THEME_NAME/" "$GHOSTTY_CONFIG"
  else
    echo "theme = $THEME_NAME" >> "$GHOSTTY_CONFIG"
  fi
fi

echo "  Ghostty theme updated"

# ============================================
# Update Starship palette
# ============================================
if [ -f "$STARSHIP_CONFIG" ]; then
  echo "Updating Starship palette..."

  # Create lowercase theme name for palette
  PALETTE_NAME=$(echo "$THEME_NAME" | tr '[:upper:]' '[:lower:]' | tr ' ' '_')

  # Update palette reference
  sed -i "s/^palette = .*/palette = '$PALETTE_NAME'/" "$STARSHIP_CONFIG"

  # Remove existing palette section and add new one
  # First, remove old palette block (from [palettes.* to next section or EOF)
  sed -i '/^\[palettes\./,/^\[/{ /^\[palettes\./d; /^\[/!d; }' "$STARSHIP_CONFIG"

  # Append new palette
  cat >> "$STARSHIP_CONFIG" << EOF

[palettes.$PALETTE_NAME]
bracket = '$COLOR_YELLOW'
time = '$COLOR_CYAN'
user = '$COLOR_MAGENTA'
at = '$COLOR_GREEN'
host = '$COLOR_GREEN'
path = '$COLOR_BLUE'
prompt = '$COLOR_YELLOW'
git = '$COLOR_CYAN'
EOF

  echo "  Starship palette updated to '$PALETTE_NAME'"
fi

# ============================================
# Update Tmux theme (tmux-power with SeaShells)
# ============================================
TMUX_CONF="$HOME/.tmux.conf"
if [ -f "$TMUX_CONF" ]; then
  echo "Updating Tmux theme..."

  # Check if theme name contains "light" (case insensitive)
  if [[ "${THEME_NAME,,}" == *"light"* ]]; then
    # SeaShells Light colors
    TMUX_TC='#50a3b5'
    TMUX_G0='#e0d6c8'
    TMUX_G1='#c8dde8'
    TMUX_G2='#b8c8d0'
    TMUX_G3='#2d6870'
    TMUX_G4='#0f2838'
    THEME_LABEL="SeaShells Light"
  else
    # SeaShells Dark colors
    TMUX_TC='#50a3b5'
    TMUX_G0='#08131a'
    TMUX_G1='#0f2838'
    TMUX_G2='#1e4862'
    TMUX_G3='#2d6870'
    TMUX_G4='#deb88d'
    THEME_LABEL="SeaShells Dark"
  fi

  # Update tmux-power colors in tmux.conf
  sed -i "s/@tmux_power_theme '[^']*'/@tmux_power_theme '$TMUX_TC'/" "$TMUX_CONF"
  sed -i "s/@tmux_power_g0 '[^']*'/@tmux_power_g0 '$TMUX_G0'/" "$TMUX_CONF"
  sed -i "s/@tmux_power_g1 '[^']*'/@tmux_power_g1 '$TMUX_G1'/" "$TMUX_CONF"
  sed -i "s/@tmux_power_g2 '[^']*'/@tmux_power_g2 '$TMUX_G2'/" "$TMUX_CONF"
  sed -i "s/@tmux_power_g3 '[^']*'/@tmux_power_g3 '$TMUX_G3'/" "$TMUX_CONF"
  sed -i "s/@tmux_power_g4 '[^']*'/@tmux_power_g4 '$TMUX_G4'/" "$TMUX_CONF"

  # Reload tmux if running
  if command -v tmux &> /dev/null && tmux list-sessions &> /dev/null 2>&1; then
    tmux source-file "$TMUX_CONF" 2>/dev/null
    echo "  Tmux: $THEME_LABEL (reloaded)"
  else
    echo "  Tmux: $THEME_LABEL (restart tmux to apply)"
  fi
fi

echo ""
echo "Theme sync complete! Restart your terminal to see changes."
