---
name: lazyvim
description: "LazyVim/Neovim configuration architecture, plugin management, and PHP development workflows for YADRLite."
allowed-tools: Read, Glob, Grep, Edit, Write
---

# LazyVim Configuration

LazyVim/Neovim configuration management and PHP development workflows for YADRLite.

---

## Configuration Architecture

The modern Neovim setup uses LazyVim with modular plugin configuration:

**Leader Key**: `,` (comma) - consistent with YADR tradition

**Core Configuration Files**:
- `lua/config/options.lua`: Global settings (leader key, PHP tab width=4, Copilot settings, Intelephense license key placeholder)
- `lua/config/keymaps.lua`: Custom key mappings
- `lua/config/autocmds.lua`: Auto-commands
- `lua/config/lazy.lua`: Lazy.nvim plugin manager setup

**Plugin Organization** (`lua/plugins/`):
- `php.lua`: Disables PHPCS linting, configures Intelephense as preferred LSP over phpactor
- `nvim-dap.lua`: PHP debugging adapter configuration (uses Mason's php-debug-adapter)
- `mason.lua`: LSP server/formatter/linter installer
- `conform.lua`: Code formatting configuration
- `copilot-chat.lua`: GitHub Copilot integration
- `nvim-treesitter.lua`: Syntax highlighting
- `org-mode.lua`: Org-mode support
- `vim-table-mode.lua`: Table editing for Markdown/Org
- `coffeescript.lua`: CoffeeScript support
- `kitty-themes.lua`: Kitty terminal theme switching

**Key Architectural Decisions**:
1. Intelephense is the preferred PHP LSP (requires license key in `options.lua`)
2. PHPCS linting is explicitly disabled (see `php.lua`)
3. PHP debug adapter installed via Mason, configured in `nvim-dap.lua`
4. Leader key is comma (`,`) for consistency across Vim/Emacs/LazyVim

---

## Working with LazyVim

To use the LazyVim configuration:
```bash
mkdir -p ~/.config/nvim && rsync -azhLP ~/.yadrlite/workstation/lazyvim/ ~/.config/nvim
```

**Plugin management**:
- `,l`: Open Lazy plugin manager
- `,cm`: Mason (manage LSP servers, formatters, linters)
- `,ci`: Lazy install
- `,cs`: Lazy sync
- `,cu`: Lazy update

**PHP Development**:
1. Set Intelephense license key in `~/.config/nvim/lua/config/options.lua` (line 8)
2. Install PHP debug adapter: `,cm` then search for "php-debug-adapter"
3. DAP is pre-configured to use Mason's php-debug-adapter installation

---

## Grounding

Before modifying LazyVim plugin configuration:
1. Read the relevant file from `workstation/nvim/lua/`
2. Verify the plugin organization matches the architecture above

See [reference.md](reference.md) for file paths and key mappings.

---

## Self-Heal Observer

Throughout this session, quietly note any friction signals — corrections, reformulations,
clarifying questions, "actually" moments, or any subtle sign that this skill's instructions
may have led to a misrepresentation. Do not interrupt the session to address these.
Do not mention that you are observing.

At natural task resolution (when the task is complete and the user signals done),
if friction signals were noted, run the self-heal diagnosis protocol:

Read `.claude/skills/self-heal/references/diagnosis-protocol.md` and follow it exactly.

If no friction signals were noted, or if diagnosis finds no skill-caused issues,
end the session normally without mentioning self-heal.

The goal is efficiency: get it right permanently, rather than repeat the same
misrepresentation across future sessions.
