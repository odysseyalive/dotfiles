# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

YADRLite is a streamlined dotfiles package based on YADR, optimized for speed and portability. It provides configurations for Vim, Emacs (with Evil mode), Tmux, LazyVim (Neovim), Kitty terminal, and Ranger file manager. The repository is primarily geared towards PHP development on local staging environments and web servers.

## Setup and Installation

See `/yadrlite-setup` skill for install, tools, ASDF language management, migrations, update, uninstall, and Arch workstation setup commands.

## Testing

See `/yadrlite-testing` skill for Makefile targets, ShellSpec, ShellCheck, and zsh syntax linting. Quick check: `make check`.

## Architecture and Structure

### Core Components

**install script** (`./install.sh`): Bootstraps the repo — clones to `~/.yadrlite`, backs up existing dotfiles to `~/.yadrlite/backup/`, creates symlinks, and sets up core tmux plugins.

**setup script** (`./setup.sh`): Post-install orchestrator that routes to focused scripts in `setup/` for actions like `tools`, `update`, `macos`, `keyboard`, and `remove`.

### Directory Layout

```
├── setup/          # Modular post-installation management scripts
├── install.sh      # Initial bootstrap script
├── setup.sh        # Setup management entrypoint
├── bash/           # Bash configuration snippets
├── emacs.d/        # Emacs configuration with Evil mode
├── vim/            # Vim configuration (vimrc, settings/, autoload/)
├── tmux/           # Tmux configuration and plugins
├── workstation/    # System-specific setup and tools
│   ├── nvim/       # LazyVim configuration (preferred modern setup)
│   ├── ranger/     # Ranger file manager config
│   ├── kitty/      # Kitty terminal config
│   └── fonts/      # Terminal fonts
└── assets/         # Screenshots
```

### Editor Configurations

**Leader key**: `,` (comma) across all editors for consistency.

See dedicated skills for detailed architecture and workflows:
- **LazyVim/Neovim**: `/lazyvim`
- **Vim**: `/vim-config`
- **Emacs**: `/emacs-config`
- **Tmux & Ranger**: `/editor-tools`

## Skills Reference

| Skill | Description |
|-------|-------------|
| `/yadrlite-setup` | Install, tools, ASDF languages, migrations, update, uninstall |
| `/yadrlite-testing` | Makefile, ShellSpec, ShellCheck, zsh syntax linting |
| `/lazyvim` | LazyVim/Neovim architecture, plugin management, PHP development |
| `/vim-config` | Vim-Plug management, tag navigation, search, Xdebug debugging |
| `/emacs-config` | Evil mode, DAP debugging, project search |
| `/editor-tools` | Tmux plugin management, Ranger configuration |
| `/awareness-ledger` | Institutional memory — incidents, decisions, patterns, flows |

## Important Configuration Notes

1. **PHP Indentation**: All configurations use 4 spaces for PHP (see LazyVim `options.lua` autocmd, line 9-17)
2. **Shell Integration**: The setup script intelligently merges bash configs by detecting whether `~/.bashrc` is sourced from `~/.bash_profile`
3. **Tmux Shell Detection**: The setup script auto-detects the user's shell and configures tmux accordingly
4. **Platform Support**: Linux, FreeBSD, and macOS. macOS requires `zsh ~/.yadrlite/setup.zsh macos` after basic install.
5. **Backup Strategy**: All original dotfiles are backed up to `~/.yadrlite/backup/` before installation
6. **Vim-Tmux Navigation**: `vim-tmux-navigator` enables `Ctrl-h/j/k/l` across Vim and Tmux panes

## Key Dependencies

**Required**: git, Vim with Lua support, Emacs 27.2+ (29 recommended, avoid 28), Tmux, Neovim 0.9+ (for LazyVim), Node.js (for language servers).

**Recommended**: Silver Searcher (`ag`) or Ripgrep (`rg`), `fd`, `fzf`, `lazygit`, Composer, PHP CS Fixer.

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
