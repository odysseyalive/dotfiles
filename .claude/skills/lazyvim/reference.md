# LazyVim Reference

## File Paths

| File | Purpose |
|------|---------|
| `workstation/nvim/init.lua` | Entry point |
| `workstation/nvim/lua/config/options.lua` | Global settings, leader key, PHP tab width |
| `workstation/nvim/lua/config/keymaps.lua` | Custom key mappings |
| `workstation/nvim/lua/config/autocmds.lua` | Auto-commands |
| `workstation/nvim/lua/config/lazy.lua` | Plugin manager setup |
| `workstation/nvim/lua/plugins/php.lua` | PHP LSP config (Intelephense) |
| `workstation/nvim/lua/plugins/nvim-dap.lua` | Debug adapter config |
| `workstation/nvim/lua/plugins/mason.lua` | LSP/formatter/linter installer |
| `workstation/nvim/lua/plugins/conform.lua` | Code formatting |
| `workstation/nvim/lua/plugins/copilot-chat.lua` | GitHub Copilot |
| `workstation/nvim/lua/plugins/nvim-treesitter.lua` | Syntax highlighting |
| `workstation/nvim/lua/plugins/org-mode.lua` | Org-mode support |
| `workstation/nvim/lua/plugins/vim-table-mode.lua` | Table editing |
| `workstation/nvim/lua/plugins/coffeescript.lua` | CoffeeScript support |
| `workstation/nvim/lua/plugins/kitty-themes.lua` | Kitty theme switching |

## Key Mappings

| Key | Action |
|-----|--------|
| `,l` | Open Lazy plugin manager |
| `,cm` | Mason (manage LSP servers) |
| `,ci` | Lazy install |
| `,cs` | Lazy sync |
| `,cu` | Lazy update |
