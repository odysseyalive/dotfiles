#!/bin/bash

# Omarchy Theme Sync for Kitty, Starship, and Tmux
# Syncs terminal colors with current Omarchy theme - live updates without restart
#
# Usage:
#   omarchy-theme-sync           # Sync themes with current Omarchy theme
#
# Install as Omarchy hook for automatic sync:
#   ln -sf ~/.yadrlite/workstation/scripts/omarchy-theme-sync.sh ~/.config/omarchy/hooks/theme-set

STARSHIP_CONFIG="$HOME/.config/starship.toml"
OMARCHY_THEME_DIR="$HOME/.config/omarchy/current/theme"
KITTY_THEME="$OMARCHY_THEME_DIR/kitty.conf"

# Check if Omarchy theme exists
if [ ! -d "$OMARCHY_THEME_DIR" ]; then
  echo "Error: Omarchy theme directory not found at $OMARCHY_THEME_DIR"
  exit 1
fi

# Get theme name from kitty.conf
THEME_NAME=""
if [ -f "$KITTY_THEME" ]; then
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
echo "Syncing theme: $THEME_NAME"

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
# Update Kitty (live reload)
# ============================================
if pgrep -x kitty > /dev/null 2>&1; then
  echo "Updating Kitty colors..."
  # Try kitty remote control first (requires allow_remote_control in kitty.conf)
  if kitty @ set-colors --all "$KITTY_THEME" 2>/dev/null; then
    echo "  Kitty: colors updated via remote control"
  else
    # Fallback: signal kitty to reload config (may not work with includes)
    pkill -SIGUSR1 kitty 2>/dev/null
    echo "  Kitty: sent reload signal (restart if colors don't update)"
  fi
else
  echo "  Kitty: not running"
fi

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
# Update Tmux theme (using colors from Omarchy theme)
# ============================================
TMUX_CONF="$HOME/.tmux.conf"
if [ -f "$TMUX_CONF" ]; then
  echo "Updating Tmux theme..."

  # Get background color to determine light/dark
  BG_COLOR=$(grep -E "^background\s+" "$KITTY_THEME" | awk '{print $2}')
  FG_COLOR=$(grep -E "^foreground\s+" "$KITTY_THEME" | awk '{print $2}')

  # Extract RGB from background to determine light/dark
  # Remove # and convert to decimal
  BG_HEX="${BG_COLOR#\#}"
  if [ ${#BG_HEX} -eq 6 ]; then
    BG_R=$((16#${BG_HEX:0:2}))
    BG_G=$((16#${BG_HEX:2:2}))
    BG_B=$((16#${BG_HEX:4:2}))
    BG_LUMINANCE=$(( (BG_R * 299 + BG_G * 587 + BG_B * 114) / 1000 ))
  else
    BG_LUMINANCE=0  # Default to dark if can't parse
  fi

  # Get additional colors for tmux bar
  # Note: In light themes, color0 may be light (inverted), so use color7 or foreground for dark text
  COLOR_BLACK=$(grep -E "^color0\s+" "$KITTY_THEME" | awk '{print $2}')
  COLOR_WHITE=$(grep -E "^color7\s+" "$KITTY_THEME" | awk '{print $2}')
  COLOR_BLACK=${COLOR_BLACK:-#0f2838}
  COLOR_WHITE=${COLOR_WHITE:-#deb88d}

  # Light theme if luminance > 128
  if [ "$BG_LUMINANCE" -gt 128 ]; then
    # Light theme: TC is used for BOTH text AND accent backgrounds
    # So TC must be dark (for text) and segment bg's must be light (for contrast)
    # Use FG_COLOR (dark) for text, not color0 (which may be light in light themes)
    TMUX_TC="$COLOR_BLUE"     # Dark teal - accent color (text & active bg)
    TMUX_G0="$BG_COLOR"       # Cream - main bar background
    TMUX_G1="$BG_COLOR"       # Cream - blends with bar
    TMUX_G2="$COLOR_CYAN"     # Light cyan - segment bg (TC text readable on this)
    TMUX_G3="$COLOR_GREEN"    # Teal - accent
    TMUX_G4="$FG_COLOR"       # Dark (foreground) - text on cream bar bg
    THEME_LABEL="Light"
  else
    # Dark theme: G0=darkest (bar bg), G4=lightest (text)
    TMUX_TC="$COLOR_CYAN"     # Cyan - text on colored segments
    TMUX_G0="$BG_COLOR"       # Dark - main bar background
    TMUX_G1="$COLOR_BLACK"    # Darker - secondary segments
    TMUX_G2="$COLOR_BLUE"     # Medium - accent segments
    TMUX_G3="$COLOR_GREEN"    # Accent
    TMUX_G4="$FG_COLOR"       # Light - text on bar
    THEME_LABEL="Dark"
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
    # Set tmux-power options in the running session
    tmux set -g @tmux_power_theme "$TMUX_TC"
    tmux set -g @tmux_power_g0 "$TMUX_G0"
    tmux set -g @tmux_power_g1 "$TMUX_G1"
    tmux set -g @tmux_power_g2 "$TMUX_G2"
    tmux set -g @tmux_power_g3 "$TMUX_G3"
    tmux set -g @tmux_power_g4 "$TMUX_G4"

    # Re-run tmux-power plugin to regenerate status bar with new colors
    TMUX_POWER_SCRIPT="$HOME/.yadrlite/tmux/plugin/tmux-power/tmux-power.tmux"
    if [ -f "$TMUX_POWER_SCRIPT" ]; then
      tmux run-shell "$TMUX_POWER_SCRIPT" 2>/dev/null
    fi

    echo "  Tmux: $THEME_LABEL (live updated)"
  else
    echo "  Tmux: $THEME_LABEL (start tmux to see changes)"
  fi
fi

# ============================================
# Update GTK/System color scheme (for Chrome and other GTK apps)
# ============================================
if [ "$BG_LUMINANCE" -gt 128 ]; then
  COLOR_SCHEME="prefer-light"
else
  COLOR_SCHEME="prefer-dark"
fi

# Set GTK color scheme via gsettings (GNOME/GTK apps including Chrome)
if command -v gsettings &> /dev/null; then
  gsettings set org.gnome.desktop.interface color-scheme "$COLOR_SCHEME" 2>/dev/null
  echo "  GTK/Chrome: $COLOR_SCHEME"
fi

# Also update via dconf for Hyprland/wlroots environments
if command -v dconf &> /dev/null; then
  dconf write /org/gnome/desktop/interface/color-scheme "'$COLOR_SCHEME'" 2>/dev/null
fi

# ============================================
# Signal Neovim to reload colorscheme
# ============================================
NVIM_THEME_TRIGGER="$HOME/.cache/nvim-theme-trigger"
mkdir -p "$(dirname "$NVIM_THEME_TRIGGER")"
echo "$THEME_NAME" > "$NVIM_THEME_TRIGGER"
echo "  Neovim: theme trigger updated (will reload on focus)"

echo ""
echo "Theme sync complete!"
