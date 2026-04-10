#!/usr/bin/env zsh
source "$(dirname "$0")/common.sh"
setopt nullglob

echo "# # Updating Dotfiles"
echo "# # # # # # # # # # # # # # # # # # # # # #"

cd "$YADR_DIR"
git pull --rebase

# Install/update tmux plugins
echo "# # Updating tmux plugins"
mkdir -p "$YADR_DIR/tmux/plugin"
mkdir -p ~/.tmux/plugins

for tplug in "${YADR_TMUX_PLUGINS[@]}"; do
  plugname=$(basename $tplug .git)
  if [ ! -d "$YADR_DIR/tmux/plugin/$plugname" ]; then
    echo "Installing $plugname..."
    git clone $tplug "$YADR_DIR/tmux/plugin/$plugname"
  fi
done

cd "$YADR_DIR/tmux/plugin"
for pdir in */(N); do
  cd "$YADR_DIR/tmux/plugin/$pdir"
  echo "Updating $pdir..."
  if [ -d ".git" ]; then
    git pull --rebase
  fi
done

# Ensure Brew environment is loaded
if [ -f "/opt/homebrew/bin/brew" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif [ -f "/usr/local/bin/brew" ]; then
  eval "$(/usr/local/bin/brew shellenv)"
elif [ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]; then
  eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# Install Starship if not present
if ! command -v starship &>/dev/null; then
  echo "# # Installing Starship prompt"
  brew install starship
fi

# Update Starship config
mkdir -p ~/.config
cp "$YADR_DIR/workstation/starship/starship.toml" ~/.config/starship.toml

# Add Starship init to shell rc if not already present
SHELL_RC=$(get_shell_rc)
if ! grep -q 'starship init zsh' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Starship prompt' >>"$SHELL_RC"
  echo 'eval "$(starship init zsh)"' >>"$SHELL_RC"
fi

# Ensure XDG tmux config exists (tmux 3.5+ checks ~/.config/tmux/ first)
mkdir -p ~/.config/tmux
ln -sf "$YADR_DIR/tmux.conf" ~/.config/tmux/tmux.conf

echo ""
echo "The update is finished."
