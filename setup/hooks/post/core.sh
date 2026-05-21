#!/usr/bin/env bash
# Core post-install hook. Sourced by setup.sh (bash) or setup.zsh (zsh).

SHELL_RC=$(get_shell_rc)

if ! grep -q 'export PATH="$HOME/.local/bin' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Local binaries' >>"$SHELL_RC"
  echo 'export PATH="$HOME/.local/bin:$PATH"' >>"$SHELL_RC"
fi

echo "# # Installing Nerd Font"
mkdir -p ~/.local/share/fonts
_font_dir="$YADR_DIR/workstation/fonts"
if [ -d "$_font_dir" ]; then
  _found_fonts=0
  for _f in "$_font_dir"/*.ttf; do
    [ -e "$_f" ] || continue
    cp "$_f" ~/.local/share/fonts/
    _found_fonts=1
  done
  if [ "$_found_fonts" -eq 1 ] && command -v fc-cache >/dev/null 2>&1; then
    fc-cache -fv ~/.local/share/fonts
  fi
fi

echo "# # Installing config files (kitty, nvim, ranger)"
mkdir -p ~/.config
mkdir -p "$YADR_DIR/backup/config" 2>/dev/null

for cfile in "${YADR_CONFIG[@]}"; do
  mv ~/.config/"$cfile" "$YADR_DIR/backup/config/$cfile" 2>/dev/null
  cp -r "$YADR_DIR/workstation/$cfile" ~/.config/"$cfile"
done
find ~/.config -type d -exec chmod 0755 {} \;
find ~/.config -type f -exec chmod 0644 {} \;
find ~/.config -name "*.sh" -execdir chmod u+x {} +

cp "$YADR_DIR/workstation/starship/starship.toml" ~/.config/starship.toml

# Starship init: pick the right initializer for the shell rc we're targeting.
_starship_shell="zsh"
case "$SHELL_RC" in
  */.bashrc|*/.bash_profile) _starship_shell="bash" ;;
esac
if ! grep -q "starship init ${_starship_shell}" "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Starship prompt' >>"$SHELL_RC"
  echo "eval \"\$(starship init ${_starship_shell})\"" >>"$SHELL_RC"
fi
