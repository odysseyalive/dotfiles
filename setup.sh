#!/usr/bin/env zsh

# yadrlite
# Workstation Management Entrypoint
# # # # # # # # # # # # # # #

action="${1:-help}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]:-${(%):-%x}}")" && pwd)"

if [ "$action" = "help" ]; then
  echo "YADRLite Setup Management"
  echo "Usage: ./setup.sh [action]"
  echo ""
  echo "Available actions:"
  echo "  tools     - Install common development tools (Node, Go, ripgrep, etc)"
  echo "  macos     - Setup macOS workstation (Ghostty, AeroSpace, Sketchybar)"
  echo "  omarchy   - Setup Arch/Omarchy workstation"
  echo "  update    - Update plugins and configurations"
  echo "  keyboard  - Swap Caps Lock and Escape keys (macOS only)"
  echo "  remove    - Uninstall YADRLite and restore backups"
  exit 0
fi

if [ -x "$SCRIPT_DIR/setup/${action}.sh" ]; then
  "$SCRIPT_DIR/setup/${action}.sh"
else
  echo "Unknown action: $action"
  echo "Run './setup.sh help' to see available actions."
  exit 1
fi
