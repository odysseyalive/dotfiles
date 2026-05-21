---
name: lazyvim
description: "LazyVim/Neovim configuration architecture, plugin management, and PHP development workflows for YADRLite."
allowed-tools: Read, Glob, Grep, Edit, Write
minimum-effort-level: high
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
1. Identify which file under `workstation/nvim/lua/` owns the plugin or setting being changed. Use the "Plugin Organization" table above to choose the file.
2. Read that file in full BEFORE issuing any Edit. IF the plugin or setting named by the user is not present in the chosen file → STOP and grep `workstation/nvim/lua/` for the symbol before proceeding; the plugin may live in a different file than expected.
3. Read [reference.md](reference.md) § "File paths and key mappings" before quoting any path or keybinding back to the user, to avoid drift between this SKILL.md and the canonical mapping table.

---

## Self-Heal Observer

Maintain a silent per-session "friction signal" tally throughout the conversation. Do not mention the tally to the user. Do not interrupt active work to address it.

CHECKPOINT — Friction Observation (runs continuously, silent):
1. After every assistant turn, scan the most recent user message for these signals:
   - The user issues a correction beginning with "actually", "no", "wait", "instead", "I meant".
   - The user reformulates a request that was already given this session.
   - The user asks a clarifying question whose answer was supposed to be supplied by THIS skill's instructions.
   - The user states the result misrepresents what they asked for.
2. For each signal observed, append `{ turn_id, signal_type, quoted_user_text }` to the in-memory tally. Do not log to disk.
3. Detect natural task resolution = (a) immediate task delivered AND (b) user said "thanks"/"done"/"ok"/"great" OR moved to a new topic AND (c) no in-flight tool calls.
4. AT natural task resolution:
   - IF tally is empty → end normally. Do not mention self-heal.
   - IF tally is non-empty → Read `.claude/skills/self-heal/references/diagnosis-protocol.md` and execute its steps literally with the tally as input.
5. IF diagnosis attributes friction to lazyvim instructions → propose the surgical correction per the diagnosis-protocol's reporting format.
6. IF diagnosis attributes friction elsewhere → end normally without mentioning self-heal.

The goal: correct the skill once, permanently, rather than repeat the same misrepresentation across future sessions.
