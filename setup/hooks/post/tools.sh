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

# tree-sitter CLI. nvim-treesitter (main) compiles parsers with it, and when
# it's missing LazyVim fetches the upstream binary through Mason. Upstream
# links that binary against glibc 2.39, so on older servers it fails with
# "GLIBC_2.39 not found". Use the prebuilt binary where it runs; otherwise
# compile it here with cargo, which links against the local glibc.
TREE_SITTER_MIN_VERSION="0.26.1"
_mason_ts="$HOME/.local/share/nvim/mason"
if [ "$(uname)" = "Linux" ]; then
  # Mason's bin dir is prepended to PATH inside nvim, so a broken Mason copy
  # would shadow a working ~/.local/bin/tree-sitter. Drop it if it can't run.
  if [ -e "$_mason_ts/bin/tree-sitter" ] && ! "$_mason_ts/bin/tree-sitter" --version >/dev/null 2>&1; then
    echo "  -> Removing Mason's tree-sitter-cli (won't run on this glibc)"
    rm -rf "$_mason_ts/bin/tree-sitter" "$_mason_ts/packages/tree-sitter-cli"
  fi

  _ts_have="$(tree-sitter --version 2>/dev/null | head -n 1 | awk '{print $2}')"
  if [ -n "$_ts_have" ] &&
    [ "$(printf '%s\n%s\n' "$TREE_SITTER_MIN_VERSION" "$_ts_have" | sort -V | head -n 1)" = "$TREE_SITTER_MIN_VERSION" ]; then
    echo "  -> tree-sitter $_ts_have is current (>= $TREE_SITTER_MIN_VERSION)"
  else
    echo "==> Installing tree-sitter CLI (found: ${_ts_have:-none}, need >= $TREE_SITTER_MIN_VERSION)"
    mkdir -p "$HOME/.local/bin"
    export PATH="$HOME/.local/bin:$PATH"
    _ts_bin="$HOME/.local/bin/tree-sitter"
    case "$(uname -m)" in
    x86_64 | amd64) _ts_arch="x64" ;;
    aarch64 | arm64) _ts_arch="arm64" ;;
    *) _ts_arch="" ;;
    esac

    if [ -n "$_ts_arch" ] && command -v curl >/dev/null 2>&1 &&
      curl -fsSL --max-time 300 "https://github.com/tree-sitter/tree-sitter/releases/latest/download/tree-sitter-linux-$_ts_arch.gz" | gunzip >"$_ts_bin.tmp" &&
      chmod +x "$_ts_bin.tmp" && "$_ts_bin.tmp" --version >/dev/null 2>&1; then
      mv -f "$_ts_bin.tmp" "$_ts_bin"
      echo "  -> Installed $("$_ts_bin" --version) (prebuilt)"
    else
      rm -f "$_ts_bin.tmp"
      if ! command -v cc >/dev/null 2>&1; then
        echo "  -> Skipping tree-sitter build (no C compiler; install gcc, which nvim-treesitter also needs)"
      else
        echo "  -> Prebuilt binary won't run here; building from source with cargo (takes a few minutes)"
        _ts_built=""
        # A distro cargo may be too old for current tree-sitter; try it first,
        # then fall back to a throwaway rustup toolchain that is removed after.
        if command -v cargo >/dev/null 2>&1 &&
          cargo install tree-sitter-cli --locked --root "$HOME/.local" &&
          "$_ts_bin" --version >/dev/null 2>&1; then
          _ts_built=1
        elif command -v curl >/dev/null 2>&1; then
          _ts_tmp="$(mktemp -d)"
          if RUSTUP_HOME="$_ts_tmp/rustup" CARGO_HOME="$_ts_tmp/cargo" sh -c '
            curl -fsSL --proto "=https" --tlsv1.2 https://sh.rustup.rs |
              sh -s -- -y --no-modify-path --profile minimal >/dev/null &&
              "$CARGO_HOME/bin/cargo" install tree-sitter-cli --locked --root "$HOME/.local"
          ' && "$_ts_bin" --version >/dev/null 2>&1; then
            _ts_built=1
          fi
          rm -rf "$_ts_tmp"
        fi
        if [ -n "$_ts_built" ]; then
          echo "  -> Installed $("$_ts_bin" --version) (built from source)"
        else
          echo "  -> tree-sitter build failed; nvim-treesitter won't be able to compile parsers"
        fi
      fi
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
