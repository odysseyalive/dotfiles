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

# Neovim. Distro repos on servers often ship an nvim too old for LazyVim,
# and npm/pnpm have no package carrying the nvim binary, so pull the official
# prebuilt release into ~/.local (no root). macOS gets nvim from Homebrew.
NVIM_MIN_VERSION="0.11.2"
if [ "$(uname)" = "Linux" ]; then
  _nvim_have="$(nvim --version 2>/dev/null | head -n 1 | sed 's/^NVIM v//; s/-.*//')"
  if [ -n "$_nvim_have" ] &&
    [ "$(printf '%s\n%s\n' "$NVIM_MIN_VERSION" "$_nvim_have" | sort -V | head -n 1)" = "$NVIM_MIN_VERSION" ]; then
    echo "  -> Neovim $_nvim_have is current (>= $NVIM_MIN_VERSION)"
  else
    case "$(uname -m)" in
    x86_64 | amd64) _nvim_arch="x86_64" ;;
    aarch64 | arm64) _nvim_arch="arm64" ;;
    *) _nvim_arch="" ;;
    esac
    if [ -z "$_nvim_arch" ]; then
      echo "  -> Skipping Neovim install (no prebuilt release for $(uname -m))"
    elif ! command -v curl >/dev/null 2>&1; then
      echo "  -> Skipping Neovim install (curl not found)"
    else
      echo "==> Installing Neovim (found: ${_nvim_have:-none}, need >= $NVIM_MIN_VERSION)"
      _nvim_opt="$HOME/.local/opt"
      mkdir -p "$_nvim_opt" "$HOME/.local/bin"
      # The main release needs a recent glibc; neovim-releases is the same
      # version built against glibc 2.17 for older enterprise distros.
      for _nvim_repo in neovim/neovim neovim/neovim-releases; do
        _nvim_url="https://github.com/$_nvim_repo/releases/latest/download/nvim-linux-$_nvim_arch.tar.gz"
        rm -rf "$_nvim_opt/nvim-linux-$_nvim_arch"
        if curl -fsSL --max-time 300 "$_nvim_url" | tar -xz -C "$_nvim_opt" &&
          "$_nvim_opt/nvim-linux-$_nvim_arch/bin/nvim" --version >/dev/null 2>&1; then
          ln -sf "$_nvim_opt/nvim-linux-$_nvim_arch/bin/nvim" "$HOME/.local/bin/nvim"
          export PATH="$HOME/.local/bin:$PATH"
          hash -r 2>/dev/null || true
          echo "  -> Installed $("$HOME/.local/bin/nvim" --version | head -n 1) from $_nvim_repo"
          break
        fi
        echo "  -> $_nvim_repo build unusable here; trying next"
        rm -rf "$_nvim_opt/nvim-linux-$_nvim_arch"
      done
    fi
  fi
fi

export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

if ! grep -q 'export GOPATH="$HOME/go"' "$SHELL_RC" 2>/dev/null; then
  echo '' >>"$SHELL_RC"
  echo '# Go paths' >>"$SHELL_RC"
  echo 'export GOPATH="$HOME/go"' >>"$SHELL_RC"
  echo 'export PATH="$GOPATH/bin:$PATH"' >>"$SHELL_RC"
fi
