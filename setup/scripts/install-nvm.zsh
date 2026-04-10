#!/usr/bin/env zsh
setopt nullglob

echo "==> Installing Node Version Manager (NVM)"
NVM_INSTALLER="$(mktemp)"
curl -fsSL https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh -o "$NVM_INSTALLER" || {
  echo "Failed to download nvm installer" >&2
  rm -f "$NVM_INSTALLER"
  exit 1
}
bash "$NVM_INSTALLER" || {
  echo "Failed to run nvm installer" >&2
  rm -f "$NVM_INSTALLER"
  exit 1
}
rm -f "$NVM_INSTALLER"

export NVM_DIR="$HOME/.nvm"
if [ ! -s "$NVM_DIR/nvm.sh" ]; then
  echo "nvm installation did not produce $NVM_DIR/nvm.sh" >&2
  exit 1
fi

source "$NVM_DIR/nvm.sh" # This loads nvm
if ! command -v nvm >/dev/null 2>&1; then
  echo "nvm is not available after installation" >&2
  exit 1
fi

nvm install v22.20.0
nvm alias default 22.20.0
nvm use 22.20.0
