# Ranger

Ranger is a console file manager with VI key bindings that provides a minimalistic interface with Miller columns layout. It's perfect for quick file navigation and preview without leaving the terminal.

```bash
mkdir -p ~/.config/ranger && rsync -azhLP ~/.yadrlite/workstation/ranger/ ~/.config/ranger
```

Requirements: [Ranger](https://ranger.github.io/)

Plugin Requirements: [w3m](http://w3m.sourceforge.net/) for image previews, [atool](https://www.nongnu.org/atool/) for archive handling

[Return to top](#yadrlite)

## Ranger Shortcuts

The ranger interface uses three columns: parent directory (left), current directory (center), and file preview (right). Navigation follows vim conventions with hjkl keys.

#### Basic Navigation

- `h,j,k,l`: Move left, down, up, right (or use arrow keys)
- `gg`: Go to top of file list
- `G`: Go to bottom of file list
- `Enter`: Open file or enter directory
- `q`: Quit ranger
- `S`: Open shell in current directory
- `Ctrl-h`: Toggle hidden files
- `zh`: Toggle hidden files (alternative)

#### File Operations

- `yy`: Copy (yank) file
- `dd`: Cut file
- `pp`: Paste file
- `Space`: Select/mark file
- `v`: Select all files
- `uv`: Unselect all files
- `cw`: Rename current file
- `A`: Rename file (cursor at end)
- `I`: Rename file (cursor at beginning)
- `:delete`: Delete selected files
- `:mkdir <name>`: Create new directory

#### Navigation Shortcuts

- `gh`: Go to home directory
- `gr`: Go to root directory
- `ge`: Go to /etc
- `gd`: Go to /dev
- `gv`: Go to /var
- `gm`: Go to /media
- `H`: Go back in history
- `L`: Go forward in history

#### File Preview and Applications

- `i`: Preview file in larger window
- `r`: Open file with application (shows menu)
- `E`: Edit file with default editor
- `:open_with <app>`: Open with specific application

#### Search and Filters

- `/`: Search for files
- `n`: Next search result
- `N`: Previous search result
- `f`: Find file (type to filter)
- `zf`: Toggle file filtering

#### Bookmarks and Tabs

- `m<letter>`: Create bookmark at current location
- `'<letter>`: Go to bookmark
- `Ctrl-n`: Create new tab
- `Ctrl-w`: Close current tab
- `Tab`: Switch to next tab
- `Shift-Tab`: Switch to previous tab

#### View Options

- `zh`: Show/hide hidden files
- `zp`: Toggle file previews
- `zi`: Toggle image previews
- `zv`: Toggle use of preview script
- `F`: Toggle freeze files (improve performance)

[Return to top](#yadrlite)

