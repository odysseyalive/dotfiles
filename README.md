# YADRLite

![Setup Demo](assets/demo.gif)

YADRLite is a highly-modular, lightning-fast dotfiles collection designed for cross-platform portability. Built on top of **Homebrew** and **Zsh**, it provides a unified configuration experience across macOS and Linux without the brittle OS-detection scripts.

It provisions modern, GPU-accelerated terminals, intelligent tiling window managers, and powerful IDE-like Neovim setups out of the box.

*Credit: This repository is heavily based on the excellent [YADR](https://github.com/skwp/dotfiles), trimmed down considerably for speed and portability. Check out the original repo if you work heavily in Python or Ruby.*

## The Architecture
YADRLite is driven by a completely dynamic feature router. It doesn't rely on monolithic bash scripts. Instead, you define exactly what you want via `--with-<feature>` flags. 

The router simply looks for `<feature>.Brewfile` bundles and `hooks/pre/<feature>.zsh` lifecycle hooks, making it infinitely scalable.

---

## ⚡ Installation

Requirements: [git](https://git-scm.com/downloads)

1. **Bootstrap the environment:**
This single command installs Homebrew (if missing), updates Git and Zsh, sets your default shell, and clones the repository.
```sh
/bin/sh -c "`curl -fsSL https://raw.githubusercontent.com/odysseyalive/dotfiles/master/install.sh`"
```

2. **Install Development Tools:**
Installs Node.js, Go tools, ripgrep, fd, starship, and essential language servers.
```zsh
zsh ~/.yadrlite/setup.zsh tools
```

---

## 🖥️ Workstation Setups

### macOS Workstation
Transform your Mac into a tiling window manager setup (no SIP disable required).

```zsh
zsh ~/.yadrlite/setup.zsh --macos --with-macos
# or use the shorthand alias:
zsh ~/.yadrlite/setup.zsh macos
```
**This installs:**
- **Ghostty** - Modern GPU-accelerated terminal with Kitty graphics protocol.
- **AeroSpace** - i3-like tiling window manager.
- **Sketchybar** - Custom status bar.
- **JankyBorders** - Window border highlighting.

### Arch / Linux Workstation
For Linux environments, specifically targeted at [Omarchy](https://omarchy.org):

```zsh
zsh ~/.yadrlite/setup.zsh --linux --with-omarchy
# or use the shorthand alias:
zsh ~/.yadrlite/setup.zsh omarchy
```

### Optional Features
- **Language Management (ASDF):** Install languages dynamically using ASDF via `--with-langs` or granular versions via `--with-lang-<name>-<version>`. If you prefer legacy installers (NVM/G-Install), use `--without-asdf`.
  ```zsh
  zsh ~/.yadrlite/setup.zsh --with-langs
  zsh ~/.yadrlite/setup.zsh --with-lang-ruby-3.2.0
  ```
- **GNU Linuxify (macOS):** Standardize your macOS coreutils to use GNU versions via Homebrew.
  ```zsh
  zsh ~/.yadrlite/setup.zsh --with-gnu
  ```
- **Keyboard Remap (macOS):** Swaps Caps Lock and Escape keys using `hidutil`.
  ```zsh
  zsh ~/.yadrlite/setup.zsh --with-keyboard
  ```

---

## 📚 Documentation & Shortcuts

We've moved the massive lists of shortcuts and editor documentation into their own dedicated guides. YADRLite includes heavily optimized configurations for:

- [LazyVim (Modern IDE Setup)](docs/lazyvim.md) - *Highly Recommended*
- [Tmux](docs/tmux.md)
- [Vim (Classic)](docs/vim.md)
- [Ranger (File Manager)](docs/ranger.md)
- [AeroSpace (macOS Window Manager)](docs/aerospace.md)
- [Ghostty (Terminal)](docs/ghostty.md)
- [Kitty (Terminal)](docs/kitty.md)
- [Hyprland (Linux Window Manager)](docs/hyprland.md)

---

## 🔄 Maintenance & Uninstallation

**Apply Rolling Migrations & Updates:**
Safely updates the repository, refreshes tmux plugins, and applies any new configuration changes automatically (e.g., migrating from legacy NVM to ASDF).
```zsh
zsh ~/.yadrlite/setup.zsh --upgrade --migrate
```

**Uninstall and Restore (Local):**
Restores your original `~/.bashrc`, `~/.zshrc`, and other configs to their pre-YADRLite state.
```sh
/bin/sh ~/.yadrlite/uninstall.sh
```

**Uninstall and Restore (Remote):**
If your local directory is broken or deleted, you can uninstall remotely:
```sh
/bin/sh -c "`curl -fsSL https://raw.githubusercontent.com/odysseyalive/dotfiles/master/uninstall.sh`"
```

*Note: To violently remove all Homebrew packages managed by YADRLite as well, pass the `--all` and `--force` flags to the uninstaller.*

---
*Maintained with [Claude Enforcer](https://github.com/odysseyalive/claude-enforcer)*
