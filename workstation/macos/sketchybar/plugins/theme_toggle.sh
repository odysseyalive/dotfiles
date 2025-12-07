#!/bin/bash

# Theme toggle plugin for Sketchybar
# Click to toggle between light and dark SeaShells themes

THEME_SWITCH="$HOME/.yadrlite/workstation/macos/scripts/theme-switch.sh"
CURRENT_THEME_FILE="$HOME/.config/current-theme"

# Get current theme
get_current_theme() {
  if [ -f "$CURRENT_THEME_FILE" ]; then
    cat "$CURRENT_THEME_FILE"
  else
    echo "seashells-dark"
  fi
}

# Get icon based on current theme
get_icon() {
  local theme=$(get_current_theme)
  if [[ "$theme" == *"light"* ]]; then
    echo ""  # Sun icon for light mode
  else
    echo ""  # Moon icon for dark mode
  fi
}

case "$SENDER" in
  "mouse.clicked")
    # Toggle theme on click
    if [ -x "$THEME_SWITCH" ]; then
      "$THEME_SWITCH" toggle
      # Update icon after toggle
      sketchybar --set $NAME icon="$(get_icon)"
    fi
    ;;
  *)
    # Update icon on load/refresh
    sketchybar --set $NAME icon="$(get_icon)"
    ;;
esac
