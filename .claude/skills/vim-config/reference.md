# Vim Configuration Reference

## File Paths

| File | Purpose |
|------|---------|
| `vim/vimrc` | Main Vim configuration |
| `vim/vimrc.after` | User overrides |
| `vim/autoload/` | Vim-Plug autoloader |
| `vim/settings/` | Modular Vim settings |

## Key Mappings

| Key | Action |
|-----|--------|
| `:PlugInstall` | Install plugins |
| `:PlugUpdate` | Update plugins |
| `:MakeTags` | Build tags |
| `,gt` | Search tag |
| `,gi` | Next in tag history |
| `,go` | Back in tag history |
| `,ag` | Project search (Silver Searcher + Fzf) |
| `,ad` | Directory search |
| `,aw` | Search current word |
| `F5` | Xdebug: run |
| `F2` | Xdebug: step over |
| `F3` | Xdebug: step into |
| `F10` | Xdebug: breakpoint |

## Dependencies

- Vim with Lua support
- Silver Searcher (ag) or Ripgrep (rg)
- fzf (fuzzy finder)
