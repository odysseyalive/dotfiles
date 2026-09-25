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

# C compiler + tree-sitter CLI. nvim-treesitter (main) and orgmode compile
# every parser with `tree-sitter build`, which calls `cc`. Neither may be
# usable on a server: the upstream CLI binary (also what Mason fetches) needs
# glibc 2.39, and shared hosts (cPanel/CloudLinux "compiler access") leave gcc
# on PATH as root:compiler 0750. Everything below installs into ~/.local
# without root, a compiler, or admin help.
TREE_SITTER_MIN_VERSION="0.26.1"
ZIG_VERSION="0.16.0"
_mason_ts="$HOME/.local/share/nvim/mason"
if [ "$(uname)" = "Linux" ]; then
  mkdir -p "$HOME/.local/bin" "$HOME/.local/opt"
  export PATH="$HOME/.local/bin:$PATH"
  case "$(uname -m)" in
  x86_64 | amd64) _arch="x86_64" _ts_arch="x64" _conda_arch="64" ;;
  aarch64 | arm64) _arch="aarch64" _ts_arch="arm64" _conda_arch="aarch64" ;;
  *) _arch="" ;;
  esac

  # A compile test, not `command -v cc`: a locked-down gcc is still on PATH.
  # Takes an optional compiler path; defaults to whatever `cc` is on PATH.
  _cc_works() {
    _cc_out="$(mktemp "$HOME/.cache/yadrlite-cc-test.XXXXXX" 2>/dev/null)" || return 1
    printf 'int main(void){return 0;}\n' | "${1:-cc}" -x c - -o "$_cc_out" >/dev/null 2>&1
    _cc_rc=$?
    rm -f "$_cc_out"
    return $_cc_rc
  }
  mkdir -p "$HOME/.cache"

  # Drop our Zig wrapper once a system cc works (e.g. compiler access was
  # granted later), so it stops shadowing gcc for pip, node-gyp and the rest.
  if grep -q 'yadrlite: Zig-backed cc' "$HOME/.local/bin/cc" 2>/dev/null; then
    _sys_cc="$(PATH="$(printf '%s' "$PATH" | tr ':' '\n' | grep -vxF "$HOME/.local/bin" | paste -sd: -)" command -v cc 2>/dev/null)"
    if [ -n "$_sys_cc" ] && _cc_works "$_sys_cc"; then
      rm -f "$HOME/.local/bin/cc"
      hash -r 2>/dev/null || true
      echo "  -> System cc works now ($_sys_cc); removed the Zig cc wrapper"
    fi
  fi

  if _cc_works; then
    echo "  -> C compiler OK ($(command -v cc))"
  elif [ -z "$_arch" ] || ! command -v curl >/dev/null 2>&1; then
    echo "  !! No usable C compiler and no way to fetch Zig; nvim can't compile parsers"
  else
    # No usable system compiler: use Zig's bundled clang as `cc`. The wrapper
    # drops the Rust-style --target triple tree-sitter passes, which zig rejects.
    echo "==> No usable C compiler; installing Zig $ZIG_VERSION as ~/.local/bin/cc"
    _zig_dir="$HOME/.local/opt/zig-$_arch-linux-$ZIG_VERSION"
    if [ ! -x "$_zig_dir/zig" ]; then
      # find, not a glob: setup.zsh sources this hook too, and an unmatched
      # glob aborts under zsh's nomatch.
      find "$HOME/.local/opt" -maxdepth 1 -name 'zig-*' -exec rm -rf {} +
      curl -fsSL --max-time 600 "https://ziglang.org/download/$ZIG_VERSION/zig-$_arch-linux-$ZIG_VERSION.tar.xz" |
        tar -xJ -C "$HOME/.local/opt"
    fi
    ln -sf "$_zig_dir/zig" "$HOME/.local/bin/zig"
    cat >"$HOME/.local/bin/cc" <<'CC_EOF'
#!/bin/sh
# yadrlite: Zig-backed cc for hosts without a usable system compiler.
for _a; do
  shift
  case $_a in --target=*) ;; *) set -- "$@" "$_a" ;; esac
done
exec zig cc "$@"
CC_EOF
    chmod +x "$HOME/.local/bin/cc"
    hash -r 2>/dev/null || true
    if _cc_works; then
      echo "  -> Installed $(zig version 2>/dev/null | sed 's/^/Zig /') as cc"
    else
      echo "  !! Zig install failed; nvim can't compile parsers"
    fi
  fi

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
  elif [ -z "$_arch" ] || ! command -v curl >/dev/null 2>&1; then
    echo "  -> Skipping tree-sitter CLI install (unsupported arch $(uname -m) or no curl)"
  else
    echo "==> Installing tree-sitter CLI (found: ${_ts_have:-none}, need >= $TREE_SITTER_MIN_VERSION)"
    _ts_bin="$HOME/.local/bin/tree-sitter"
    _ts_env="$HOME/.local/opt/tree-sitter"
    rm -f "$_ts_bin"
    if curl -fsSL --max-time 300 "https://github.com/tree-sitter/tree-sitter/releases/latest/download/tree-sitter-linux-$_ts_arch.gz" | gunzip >"$_ts_bin.tmp" &&
      chmod +x "$_ts_bin.tmp" && "$_ts_bin.tmp" --version >/dev/null 2>&1; then
      mv -f "$_ts_bin.tmp" "$_ts_bin"
      rm -rf "$_ts_env"
      echo "  -> Installed $("$_ts_bin" --version) (upstream prebuilt)"
    else
      # Upstream needs glibc 2.39. conda-forge builds the same CLI against
      # glibc 2.17; a throwaway static micromamba fetches it (seconds, ~14 MB).
      rm -f "$_ts_bin.tmp"
      echo "  -> Upstream binary won't run on this glibc; using the conda-forge build"
      _mm_tmp="$HOME/.cache/yadrlite-micromamba"
      rm -rf "$_mm_tmp" "$_ts_env"
      mkdir -p "$_mm_tmp"
      if curl -fsSL --max-time 300 -o "$_mm_tmp/micromamba" "https://github.com/mamba-org/micromamba-releases/releases/latest/download/micromamba-linux-$_conda_arch" &&
        chmod +x "$_mm_tmp/micromamba" &&
        "$_mm_tmp/micromamba" create -y -q -r "$_mm_tmp/root" -p "$_ts_env" \
          -c conda-forge --override-channels tree-sitter-cli >/dev/null &&
        ln -sf "$_ts_env/bin/tree-sitter" "$_ts_bin" && "$_ts_bin" --version >/dev/null 2>&1; then
        echo "  -> Installed $("$_ts_bin" --version) (conda-forge)"
      else
        rm -f "$_ts_bin"
        echo "  !! tree-sitter CLI install failed; nvim-treesitter won't be able to compile parsers"
      fi
      rm -rf "$_mm_tmp"
    fi
  fi

  # Parsers compile with whichever tree-sitter comes first on PATH, so a broken
  # copy earlier on PATH (e.g. an npm tree-sitter-cli) still breaks nvim.
  _ts_first="$(command -v tree-sitter 2>/dev/null)"
  if [ -n "$_ts_first" ] && ! "$_ts_first" --version >/dev/null 2>&1; then
    echo "  !! $_ts_first comes first on PATH and won't run on this glibc; remove it"
  fi
fi

# Starship. core.sh wires `starship init` into the shell rc, but headless
# installs skip Homebrew, so the binary is missing there. Upstream's musl
# build is statically linked and runs regardless of the server's glibc.
if [ "$(uname)" = "Linux" ]; then
  if command -v starship >/dev/null 2>&1; then
    echo "  -> $(starship --version | head -n 1) already installed"
  else
    case "$(uname -m)" in
    x86_64 | amd64) _starship_arch="x86_64" ;;
    aarch64 | arm64) _starship_arch="aarch64" ;;
    *) _starship_arch="" ;;
    esac
    if [ -z "$_starship_arch" ]; then
      echo "  -> Skipping Starship install (no prebuilt release for $(uname -m))"
    elif ! command -v curl >/dev/null 2>&1; then
      echo "  -> Skipping Starship install (curl not found)"
    else
      echo "==> Installing Starship"
      mkdir -p "$HOME/.local/bin"
      _starship_url="https://github.com/starship/starship/releases/latest/download/starship-$_starship_arch-unknown-linux-musl.tar.gz"
      if curl -fsSL --max-time 300 "$_starship_url" | tar -xz -C "$HOME/.local/bin" starship &&
        "$HOME/.local/bin/starship" --version >/dev/null 2>&1; then
        export PATH="$HOME/.local/bin:$PATH"
        echo "  -> Installed $("$HOME/.local/bin/starship" --version | head -n 1)"
      else
        rm -f "$HOME/.local/bin/starship"
        echo "  -> Starship install failed; `starship init` in the shell rc will error until it is installed"
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
