# Emacs Configuration Reference

## File Paths

| File | Purpose |
|------|---------|
| `emacs.d/emacs.init` | Main Emacs initialization file (55k+ lines) |
| `~/.yadrlite/emacs.d/.extension/` | vscode-php-debug extension directory |

## Key Mappings

| Key | Action |
|-----|--------|
| `Ctrl-z` | Toggle Evil/Emacs mode |
| `,dd` | Start debugger |
| `,db` | Toggle breakpoint |
| `,dc` | Continue |
| `,di` | Step into |
| `,dp` | Install vscode-php-debug plugin |
| `,ps` | Search in current project |
| `,pd` | Search in directory |
| `,/` | Search current project |
| `,pf` | Fuzzy file finder |

## Dependencies

- Emacs 27.2+ with Lua support (avoid 28, 29 recommended)
