#!/bin/sh

# yadrlite
# Initial Installation Bootstrap
# # # # # # # # # # # # # # #

dir="$HOME/.yadrlite"
dotfiles_old="backup"
files="vim vimrc tmux.conf bash_profile bashrc vimrc.after"
tmuxplugins="https://github.com/tmux-plugins/tmux-resurrect.git https://github.com/tmux-plugins/tmux-sensible https://github.com/wfxr/tmux-power.git"

sed_i() {
  if [ "$(uname)" = "Darwin" ]; then
    sed -i '' "$@"
  else
    sed -i "$@"
  fi
}

echo "# # Installing Dotfiles"
echo "# # # # # # # # # # # # # # # # # # # # # #"

if [ -d "$dir/.git" ]; then
  echo ""
  echo "YADRLite is already installed in $dir."
  echo "Use ~/.yadrlite/setup.sh to manage your installation."
  exit 0
fi

# Ensure Homebrew is installed and available
if ! command -v brew >/dev/null 2>&1; then
  if [ -f "/opt/homebrew/bin/brew" ]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
  elif [ -f "/usr/local/bin/brew" ]; then
    eval "$(/usr/local/bin/brew shellenv)"
  elif [ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
  else
    echo "Homebrew not found. Installing Homebrew (supports macOS and Linux)..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    if [ -f "/opt/homebrew/bin/brew" ]; then
      eval "$(/opt/homebrew/bin/brew shellenv)"
    elif [ -f "/usr/local/bin/brew" ]; then
      eval "$(/usr/local/bin/brew shellenv)"
    elif [ -f "/home/linuxbrew/.linuxbrew/bin/brew" ]; then
      eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    fi
  fi
fi

echo "Ensuring Git and Zsh are installed via Homebrew..."
brew install git zsh

BREW_ZSH="$(brew --prefix)/bin/zsh"
if ! grep -q "$BREW_ZSH" /etc/shells 2>/dev/null; then
  echo "Authorizing Homebrew Zsh in /etc/shells..."
  echo "$BREW_ZSH" | sudo tee -a /etc/shells >/dev/null
fi

if [ "$SHELL" != "$BREW_ZSH" ]; then
  echo "Changing default shell to Homebrew Zsh..."
  chsh -s "$BREW_ZSH"
fi

if ! command -v git >/dev/null 2>&1; then
  echo ""
  echo "'Git' doesn't seem to be installed. Installation failed."
  exit 1
fi

# We are bootstrapping from curl, clone the repo
git clone https://github.com/odysseyalive/dotfiles.git "$dir"

echo "# # Backing up current configurations"
echo "# # # # # # # # # # # # # # # # # # # # # #"
cd "$dir" || exit 1
cat ~/.bashrc >"$dir/bashrc" 2>/dev/null
cat ~/.bash_profile >"$dir/bash_profile" 2>/dev/null
mkdir -p "$dotfiles_old" 2>/dev/null
for cfile in $files; do
  mv ~/."$cfile" "$dir/$dotfiles_old/" 2>/dev/null
done

echo "# # Vim and Tmux Configurations"
echo "# # # # # # # # # # # # # # # # # # # # # #"
cat "$dir/vim/vimrc" >>"$dir/vimrc" 2>/dev/null
cat "$dir/vim/vimrc.after" >>"$dir/vimrc.after" 2>/dev/null
cat "$dir/tmux/tmux.conf" >"$dir/tmux.conf"

OS="$(uname)"
if [ "$OS" = "Linux" ] || [ "$OS" = "FreeBSD" ]; then
  echo "Gnu/Linux detected..."
elif [ "$OS" = "Darwin" ]; then
  echo "macOS detected..."
else
  rm -rf "$dir"
  echo "Your OS isn't supported. YADRLite supports Linux, FreeBSD, and macOS."
  exit 1
fi

echo "# # Loading plugins"
echo "# # # # # # # # # # # # # # # # # # # # # #"
mkdir "$dir/tmux/plugin" 2>/dev/null
mkdir -p ~/.tmux/plugins 2>/dev/null
cd "$dir/tmux/plugin" || exit 1
for tplug in $tmuxplugins; do
  echo "git clone $tplug"
  git clone "$tplug"
done

# covers injection into most existing configurations
if ! grep -q "$HOME/.bashrc" "$dir/bash_profile" && ! grep -q "$HOME/.bash_profile" "$dir/bashrc"; then
  cat "$dir/bash/bashrc" >>"$dir/bashrc" 2>/dev/null
  echo "source $HOME/.bashrc" >>"$dir/bash_profile" 2>/dev/null
elif ! grep -q "$HOME/.bashrc" "$dir/bash_profile"; then
  cat "$dir/bash/bashrc" >>"$dir/bash_profile" 2>/dev/null
else
  cat "$dir/bash/bashrc" >>"$dir/bashrc" 2>/dev/null
fi

# fixes sourcing of bashrc within tmux
sed_i "s@.*\..*/etc/bashrc@    source /etc/bashrc@g" "$dir/bashrc"
sed_i "s@.*\..*~\.bashrc@    source ~/.bashrc@g" "$dir/bash_profile"

echo "# # Building Symbolic Links"
echo "# # # # # # # # # # # # # # # # # # # # # #"
cd "$dir" || exit 1
mkdir -p "$dotfiles_old" 2>/dev/null
for cfile in $files; do
  ln -s "$dir/$cfile" ~/."$cfile"
done

# XDG tmux config (tmux 3.5+ checks ~/.config/tmux/ before ~/.tmux.conf)
mkdir -p ~/.config/tmux
ln -sf "$dir/tmux.conf" ~/.config/tmux/tmux.conf

# Ensure Homebrew environment variables are added to ~/.zshrc if they aren't already there
if [ -f "/opt/homebrew/bin/brew" ] && ! grep -q "eval \"\$(/opt/homebrew/bin/brew shellenv)\"" ~/.zshrc 2>/dev/null; then
  echo "eval \"\$(/opt/homebrew/bin/brew shellenv)\"" >> ~/.zshrc
elif [ -f "/usr/local/bin/brew" ] && ! grep -q "eval \"\$(/usr/local/bin/brew shellenv)\"" ~/.zshrc 2>/dev/null; then
  echo "eval \"\$(/usr/local/bin/brew shellenv)\"" >> ~/.zshrc
elif [ -f "/home/linuxbrew/.linuxbrew/bin/brew" ] && ! grep -q "eval \"\$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)\"" ~/.zshrc 2>/dev/null; then
  echo "eval \"\$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)\"" >> ~/.zshrc
fi

echo ""
echo "The dotfiles are installed!"
echo ""
echo "Next steps:"
echo "  1. Install development tools:  zsh ~/.yadrlite/setup.sh tools"
echo "  2. Restart your terminal (now using Zsh!)"
echo ""
echo "Optional setups:"
echo "  macOS workstation:     zsh ~/.yadrlite/setup.sh macos"
echo "  Omarchy (Arch) setup:  zsh ~/.yadrlite/setup.sh omarchy"
echo "  Swap CapsLock/Escape:  zsh ~/.yadrlite/setup.sh keyboard"
echo ""
