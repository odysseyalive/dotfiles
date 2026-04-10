#!/bin/sh

# yadrlite
# Uninstallation Script
# # # # # # # # # # # # # # #

dir="$HOME/.yadrlite"
dotfiles_old="backup"
files="vim vimrc tmux.conf bash_profile bashrc vimrc.after"
configs="kitty nvim ranger"

FORCE=0
REMOVE_ALL=0

for arg in "$@"; do
  case $arg in
    --force) FORCE=1 ;;
    --all) REMOVE_ALL=1 ;;
    -h|--help)
      echo "Usage: ./uninstall.sh [--force] [--all]"
      echo "  --force   Bypass confirmation prompts"
      echo "  --all     Also uninstall Homebrew packages managed by YADRLite"
      exit 0
      ;;
  esac
done

if [ "$FORCE" -eq 0 ]; then
  printf "Are you sure you want to completely remove YADRLite? [y/N] "
  read -r response
  case "$response" in
    [yY][eE][sS]|[yY]) ;;
    *) echo "Uninstallation cancelled."; exit 0 ;;
  esac
fi

echo "==> Restoring dotfiles from backup..."
for cfile in $files; do
  rm -rf "$HOME/.$cfile" 2>/dev/null
  if [ -e "$dir/$dotfiles_old/.$cfile" ]; then
    mv "$dir/$dotfiles_old/.$cfile" "$HOME/" 2>/dev/null
  fi
done

echo "==> Restoring config directories from backup..."
for cfile in $configs; do
  rm -rf "$HOME/.config/$cfile" 2>/dev/null
  if [ -e "$dir/$dotfiles_old/config/$cfile" ]; then
    mv "$dir/$dotfiles_old/config/$cfile" "$HOME/.config/$cfile" 2>/dev/null
  fi
done

if [ "$REMOVE_ALL" -eq 1 ]; then
  echo "==> Uninstalling Homebrew packages managed by YADRLite..."
  if command -v brew >/dev/null 2>&1; then
    # Extract package names from Brewfiles
    PACKAGES=$(awk -F'"' '/^(brew|cask)/ {print $2}' "$dir"/brewfiles/*.Brewfile 2>/dev/null)
    if [ -n "$PACKAGES" ]; then
      # shellcheck disable=SC2086
      brew uninstall --force $PACKAGES
    fi
  else
    echo "Homebrew not found in PATH, skipping package uninstallation."
  fi
fi

echo "==> Removing YADRLite directory..."
rm -rf "$dir"

echo "==> Uninstallation complete."
echo "Please restart your terminal or source your shell config."
