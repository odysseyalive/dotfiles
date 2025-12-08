#!/bin/bash

# SeaShells Theme Switcher for macOS
# Switches between light and dark themes for Ghostty, Sketchybar, JankyBorders, and Starship
#
# Usage:
#   theme-switch.sh dark      # Switch to seashells-dark
#   theme-switch.sh light     # Switch to seashells-light
#   theme-switch.sh toggle    # Toggle between themes
#   theme-switch.sh           # Show current theme

THEME_DIR="$HOME/.config/themes"
CURRENT_THEME_FILE="$HOME/.config/current-theme"
STARSHIP_CONFIG="$HOME/.config/starship.toml"
YADRLITE_THEMES="$HOME/.yadrlite/workstation/macos/themes"

# Ensure theme directory exists
mkdir -p "$THEME_DIR"

# Get current theme
get_current_theme() {
  if [ -f "$CURRENT_THEME_FILE" ]; then
    cat "$CURRENT_THEME_FILE"
  else
    echo "seashells-dark"
  fi
}

# Set theme
set_theme() {
  local theme="$1"
  local theme_path=""

  # Determine full theme name
  case "$theme" in
    dark|seashells-dark)
      theme="seashells-dark"
      ;;
    light|seashells-light)
      theme="seashells-light"
      ;;
    *)
      echo "Unknown theme: $theme"
      echo "Available themes: dark (seashells-dark), light (seashells-light)"
      exit 1
      ;;
  esac

  # Find theme path
  if [ -d "$YADRLITE_THEMES/$theme" ]; then
    theme_path="$YADRLITE_THEMES/$theme"
  elif [ -d "$THEME_DIR/$theme" ]; then
    theme_path="$THEME_DIR/$theme"
  else
    echo "Theme not found: $theme"
    exit 1
  fi

  echo "Switching to $theme..."

  # Update Ghostty
  update_ghostty "$theme_path"

  # Update Sketchybar
  update_sketchybar "$theme_path"

  # Update JankyBorders
  update_borders "$theme_path"

  # Update Neovim background
  update_neovim "$theme"

  # Update Starship palette
  update_starship "$theme_path"

  # Save current theme
  echo "$theme" > "$CURRENT_THEME_FILE"

  echo "Theme switched to $theme"
}

# Update Ghostty configuration
update_ghostty() {
  local theme_path="$1"
  local ghostty_config="$HOME/.config/ghostty/config"
  local ghostty_theme="$theme_path/ghostty.conf"

  if [ -f "$ghostty_theme" ]; then
    # Create base config if it doesn't exist
    if [ ! -f "$ghostty_config.base" ]; then
      # Extract non-color settings from current config
      grep -v -E "^(background|foreground|cursor|selection|palette)" "$ghostty_config" > "$ghostty_config.base" 2>/dev/null || true
    fi

    # Combine base config with theme colors
    cat "$ghostty_config.base" > "$ghostty_config" 2>/dev/null || true
    echo "" >> "$ghostty_config"
    echo "# Theme: $(basename $theme_path)" >> "$ghostty_config"
    cat "$ghostty_theme" >> "$ghostty_config"

    # Ghostty watches config file for changes - no reload needed
    echo "  Ghostty: Updated"
  fi
}

# Update Sketchybar
update_sketchybar() {
  local theme_path="$1"
  local sketchybar_theme="$theme_path/sketchybar.conf"

  if [ -f "$sketchybar_theme" ]; then
    # Source the theme colors
    source "$sketchybar_theme"

    # Update bar color
    sketchybar --bar color="$BAR_COLOR" 2>/dev/null

    # Update default colors
    sketchybar --default icon.color="$ICON_COLOR" label.color="$LABEL_COLOR" 2>/dev/null

    # Update specific items
    sketchybar --set apple.logo icon.color="$ACCENT_COLOR" 2>/dev/null
    sketchybar --set space_indicator icon.color="$ACCENT_COLOR" 2>/dev/null

    echo "  Sketchybar: Updated"
  fi
}

# Update JankyBorders
update_borders() {
  local theme_path="$1"
  local borders_theme="$theme_path/borders.conf"

  if [ -f "$borders_theme" ]; then
    source "$borders_theme"

    # Restart borders with new colors
    if pgrep -x "borders" > /dev/null; then
      borders active_color="$BORDER_ACTIVE" \
              inactive_color="$BORDER_INACTIVE" \
              width="${BORDER_WIDTH:-2.0}" \
              hidpi=on \
              style=round 2>/dev/null &
      echo "  JankyBorders: Updated"
    fi
  fi
}

# Update Neovim background
update_neovim() {
  local theme="$1"
  local bg="dark"

  if [[ "$theme" == *"light"* ]]; then
    bg="light"
  fi

  # Send command to all running Neovim instances via socket
  for sock in /tmp/nvim.*/0; do
    if [ -S "$sock" ]; then
      nvim --server "$sock" --remote-send "<Cmd>set background=$bg<CR>" 2>/dev/null || true
    fi
  done

  # Also try the default socket location
  if [ -S "/tmp/nvim.sock" ]; then
    nvim --server "/tmp/nvim.sock" --remote-send "<Cmd>set background=$bg<CR>" 2>/dev/null || true
  fi

  echo "  Neovim: Background set to $bg"
}

# Update Starship palette
update_starship() {
  local theme_path="$1"
  local colors_file="$theme_path/colors.sh"

  if [ -f "$colors_file" ] && [ -f "$STARSHIP_CONFIG" ]; then
    # Source theme colors
    source "$colors_file"

    # Use theme colors or fallback to defaults
    local bracket="${ACCENT:-#fba02f}"
    local time="${ACCENT:-#50a3b5}"
    local user="${FG:-#68d3f0}"
    local at="${ACCENT:-#027b9b}"
    local host="${ACCENT:-#027b9b}"
    local path="${BORDER_ACTIVE:-#2d6870}"
    local prompt="${ACCENT:-#fba02f}"
    local git="${ACCENT:-#50a3b5}"

    # Create palette name from theme
    local palette_name=$(basename "$theme_path" | tr '-' '_')

    # Update palette reference
    sed -i '' "s/^palette = .*/palette = '$palette_name'/" "$STARSHIP_CONFIG"

    # Remove existing palette section
    sed -i '' '/^\[palettes\./,/^\[/{ /^\[palettes\./d; /^\[/!d; }' "$STARSHIP_CONFIG"

    # Append new palette
    cat >> "$STARSHIP_CONFIG" << EOF

[palettes.$palette_name]
bracket = '$bracket'
time = '$time'
user = '$user'
at = '$at'
host = '$host'
path = '$path'
prompt = '$prompt'
git = '$git'
EOF

    echo "  Starship: Palette updated to '$palette_name'"
  fi
}

# Toggle theme
toggle_theme() {
  local current=$(get_current_theme)
  if [[ "$current" == *"dark"* ]]; then
    set_theme "light"
  else
    set_theme "dark"
  fi
}

# Main
case "${1:-}" in
  dark|seashells-dark)
    set_theme "dark"
    ;;
  light|seashells-light)
    set_theme "light"
    ;;
  toggle)
    toggle_theme
    ;;
  "")
    echo "Current theme: $(get_current_theme)"
    echo ""
    echo "Usage: $(basename $0) [dark|light|toggle]"
    ;;
  *)
    set_theme "$1"
    ;;
esac
