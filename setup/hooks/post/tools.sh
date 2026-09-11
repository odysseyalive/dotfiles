#!/usr/bin/env bash
# Tools post-install hook. Installs node tooling and Go paths.

SHELL_RC=$(get_shell_rc)

# Ensure ASDF is loaded in this sub-shell session if it exists so npm works.
if command -v brew >/dev/null 2>&1; then
  ASDF_DIR="$(brew --prefix asdf 2>/dev/null)/libexec"
  if [ -f "$ASDF_DIR/asdf.sh" ]; then
    # shellcheck disable=SC1090
    . "$ASDF_DIR/asdf.sh"
  fi
fi

echo "==> Configuring Global Packages & Tooling"

if command -v npm >/dev/null 2>&1; then
  NODE_PKGS="grunt-cli gulp gulp-cli csslint typescript typescript-language-server intelephense yaml-lint eslint-plugin-toml eslint-plugin-markdown golangci-lint vscode-css-languageserver-bin js-beautify unified-language-server eslint emmet-ls babel-eslint typescript-lsp coffeescript coffeelint neovim"

  # Prefer pnpm; try to install it via npm if it's missing.
  if ! command -v pnpm >/dev/null 2>&1; then
    npm install -g pnpm || true
  fi

  if command -v pnpm >/dev/null 2>&1; then
    # shellcheck disable=SC1090
    [ -s "$SHELL_RC" ] && . "$SHELL_RC"
    pnpm setup
    # Re-source so PNPM_HOME added by `pnpm setup` is on PATH for the install.
    # shellcheck disable=SC1090
    [ -s "$SHELL_RC" ] && . "$SHELL_RC"
    # shellcheck disable=SC2086
    pnpm install -g $NODE_PKGS
  else
    # Fallback: no pnpm available (e.g. headless server where the global
    # pnpm install failed) — install the same packages straight from npm.
    echo "  -> pnpm unavailable; falling back to npm for global packages"
    # shellcheck disable=SC1090
    [ -s "$SHELL_RC" ] && . "$SHELL_RC"
    # shellcheck disable=SC2086
    npm install -g $NODE_PKGS
  fi
else
  echo "  -> Skipping Node.js tooling setup (npm not found)"
fi

export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

if ! grep -q 'export GOPATH="$HOME/go"' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Go paths' >>"$SHELL_RC"
  echo 'export GOPATH="$HOME/go"' >>"$SHELL_RC"
  echo 'export PATH="$GOPATH/bin:$PATH"' >>"$SHELL_RC"
fi
