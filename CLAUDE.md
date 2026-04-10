# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

YADRLite is a streamlined dotfiles package based on YADR, optimized for speed and portability. It provides configurations for Vim, Emacs (with Evil mode), Tmux, LazyVim (Neovim), Kitty terminal, and Ranger file manager. The repository is primarily geared towards PHP development on local staging environments and web servers.

## Setup and Installation Commands

### Initial Installation
```bash
bash -c "`curl -fsSL https://raw.githubusercontent.com/odysseyalive/dotfiles/master/setup`"
```

### Install Development Tools
```bash
bash ~/.yadrlite/setup tools
```

This installs Node.js (via nvm), Go tools, and essential language servers including:
- TypeScript/JavaScript tools (eslint, babel-eslint, js-beautify)
- PHP tools (intelephense)
- Go utilities (fzf, lazygit, gitleaks, glow)

### Update Dotfiles
```bash
bash ~/.yadrlite/setup update
```

### Uninstall and Restore
```bash
bash ~/.yadrlite/setup remove
```

### Arch Linux Workstation Setup
```bash
# For Omarchy
curl https://raw.githubusercontent.com/odysseyalive/dotfiles/master/workstation/omarchy-latest > ~/install
bash install

# For Endeavorous
curl https://raw.githubusercontent.com/odysseyalive/dotfiles/master/workstation/endeavouros-mercury-neo > ~/install
bash install
```

## Architecture and Structure

### Core Components

**setup script** (`./setup`): Main installation orchestrator that:
- Backs up existing dotfiles to `~/.yadrlite/backup/`
- Aggregates configurations from subdirectories (vim/, tmux/, bash/, emacs.d/)
- Creates symbolic links in home directory for: `.vim`, `.vimrc`, `.tmux.conf`, `.profile`, `.bash_profile`, `.bashrc`, `.vimrc.after`, `.emacs`, `.emacs.d`
- Copies configurations to `~/.config/` for: nvim, ranger
- Clones and sets up tmux plugins (tmux-resurrect, tmux-sensible)
- Supports actions: `install`, `update`, `remove`, `tools`, `omarchy`, `macos`, `keyboard`

### Directory Layout

```
├── bash/           # Bash configuration snippets injected into .bashrc/.bash_profile
├── emacs.d/        # Emacs configuration with Evil mode
│   └── emacs.init  # Main Emacs initialization file (55k+ lines)
├── vim/            # Vim configuration
│   ├── autoload/   # Vim-Plug autoloader
│   ├── settings/   # Modular Vim settings
│   ├── vimrc       # Main Vim configuration
│   └── vimrc.after # User overrides
├── tmux/           # Tmux configuration and plugins
│   ├── tmux.conf   # Base tmux config
│   └── plugin/     # Tmux plugins directory
├── workstation/    # System-specific installations and tools
│   ├── nvim/       # LazyVim configuration (preferred modern setup)
│   │   ├── lua/
│   │   │   ├── config/     # Core config (options, keymaps, autocmds)
│   │   │   └── plugins/    # Plugin configurations
│   │   └── init.lua
│   ├── ranger/     # Ranger file manager config
│   ├── kitty/      # Kitty terminal config
│   ├── fonts/      # Terminal fonts
│   ├── extension/  # VSCode extensions (xdebug)
│   └── debian-bookworm, endeavouros-mercury-neo, omarchy-latest # Distro setup scripts
└── assets/         # Screenshots
```

### Editor Configurations

See dedicated skills for detailed architecture and workflows:
- **LazyVim/Neovim**: `/lazyvim` — plugin architecture, PHP development, key mappings
- **Vim**: `/vim-config` — Vim-Plug management, tag navigation, search, Xdebug
- **Emacs**: `/emacs-config` — Evil mode, DAP debugging, project search
- **Tmux & Ranger**: `/editor-tools` — plugin management, session persistence

**Leader key**: `,` (comma) across all editors for consistency.

## Development Workflows

See the dedicated skills above for editor-specific workflows (`/lazyvim`, `/vim-config`, `/emacs-config`, `/editor-tools`).

## Skills Reference

| Skill | Description |
|-------|-------------|
| `/lazyvim` | LazyVim/Neovim architecture, plugin management, PHP development |
| `/vim-config` | Vim-Plug management, tag navigation, search, Xdebug debugging |
| `/emacs-config` | Evil mode, DAP debugging, project search |
| `/editor-tools` | Tmux plugin management, Ranger configuration |
| `/awareness-ledger` | Institutional memory — incidents, decisions, patterns, flows |

## Important Configuration Notes

1. **PHP Indentation**: All configurations use 4 spaces for PHP (see LazyVim `options.lua` autocmd, line 9-17)

2. **Shell Integration**: The setup script intelligently merges bash configurations by detecting whether `~/.bashrc` is sourced from `~/.bash_profile`

3. **Tmux Shell Detection**: The setup script automatically detects the user's shell and configures tmux accordingly (line 168-170 in setup)

4. Platform Support: Linux, FreeBSD, and macOS are supported. macOS workstation setup requires running `bash ~/.yadrlite/setup macos` after the basic installation.

5. **Backup Strategy**: All original dotfiles are backed up to `~/.yadrlite/backup/` before installation

6. **Vim-Tmux Navigation**: Uses vim-tmux-navigator plugin for seamless split navigation with `Ctrl-h/j/k/l` across Vim and Tmux panes

## Key Dependencies

**Required**:
- git (for installation)
- Vim with Lua support
- Emacs 27.2+ with Lua support (avoid 28, 29 recommended)
- Tmux
- Neovim 0.9+ (for LazyVim)
- Node.js (for language servers)

**Recommended**:
- Silver Searcher (ag) or Ripgrep (rg)
- fd (for LazyVim file finding)
- fzf (fuzzy finder)
- lazygit (Git UI)
- Composer (PHP dependency manager)
- PHP CS Fixer (code formatting)

## Project Memory
This project uses an awareness ledger for institutional memory.

**Before recommending changes:** During research and planning, check
`.claude/skills/awareness-ledger/ledger/index.md` for relevant records. If
matching records exist, read them and factor their warnings, decisions, and
patterns into your recommendation. Use `/awareness-ledger consult` for full
agent-assisted analysis when high-risk overlap is detected.

**After resolving issues:** When you encounter bug investigations with root
causes, architectural decisions with trade-offs, or recurring patterns, ask
the user if they want to record the knowledge in the awareness ledger. Use
`/awareness-ledger record [type]` to capture it. Always finish the immediate
work first — suggest capture after, not during.

## Testing

The repository does not include automated tests. Manual testing involves:
1. Installing in a clean environment
2. Verifying symbolic links in home directory
3. Testing plugin installations (`:PlugInstall` for Vim, Lazy sync for LazyVim)
4. Confirming tmux plugin functionality
5. Testing removal/restore with `bash ~/.yadrlite/setup remove`
