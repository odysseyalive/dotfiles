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

  # Global installs must never need root. If npm's current global prefix
  # isn't writable (e.g. a system/SCL node like /opt/rh/... on a shared or
  # headless host), redirect it to a user-owned prefix and put it on PATH.
  _npm_prefix="$(npm config get prefix 2>/dev/null)"
  if [ -z "$_npm_prefix" ] || [ ! -w "$_npm_prefix" ]; then
    _npm_prefix="$HOME/.npm-global"
    mkdir -p "$_npm_prefix/bin"
    npm config set prefix "$_npm_prefix"
    export PATH="$_npm_prefix/bin:$PATH"
    if ! grep -q '\.npm-global/bin' "$SHELL_RC" 2>/dev/null; then
      {
        echo ''
        echo '# npm global packages (user-owned prefix)'
        echo "export PATH=\"$_npm_prefix/bin:\$PATH\""
      } >>"$SHELL_RC"
    fi
    echo "  -> Using user-owned npm prefix: $_npm_prefix"
  fi

  # pnpm needs Node >= 18. Prefer it when usable; otherwise fall back to npm
  # for the same package set instead of failing on an old system node.
  _node_major="$(node -p 'process.versions.node.split(".")[0]' 2>/dev/null || echo 0)"
  if ! command -v pnpm >/dev/null 2>&1 && [ "${_node_major:-0}" -ge 18 ]; then
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
    # No usable pnpm (missing, or Node too old) — install the same packages
    # straight from npm into the user-owned prefix.
    echo "  -> pnpm unavailable; using npm for global packages"
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
