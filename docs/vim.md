# Vim

This configuration uses [Vim-Plug](https://github.com/junegunn/vim-plug). To
initialize the plugins, enter Normal mode and type `:PlugInstall`. Similarly,
you use `:PlugUpdate` to update your plugins.

Requirements: [Vim w/ Lua support](https://www.vim.org)

Plugin Requirements: [Composer](https://getcomposer.org/),
[neovim](https://neovim.io/), [nodejs](https://nodejs.org/),
[Python 3+](https://www.python.org), [Pynvim](https://github.com/neovim/pynvim),
[Silver Searcher](https://github.com/ggreer/the_silver_searcher)

[Return to top](#yadrlite)

## Vim Shortcuts

#### Window Navigation

- `Cmd-[1-]`: (`Alt-[1-9]`) switches to a specific tab number (like iTerm and
  Chrome) and tabs have been set up to show numbers
- `Ctrl-h,l,j,k`: to move left, right, down, up between splits. This also works
  between vim and tmux splits thanks to `vim-tmux-navigator`.
- `Ctrl-w r`: rotate windows around
- `vv`: vertical split (`Ctrl-w,v`)
- `ss`: horizontal split (`Ctrl-w,s`)
- `,c`: closes window only (`Ctrl-w,c`)
- `,qo`: open quickfix window (this is where output from Grep goes)
- `,qc`: close quickfix
- `Cmd-[Up,Down,Left,Right]`: (`Alt-[Up,Down,Left,Right]`) increase hight,
  decrease height, contract window and expand window
- `=`: make all windows equal size

#### Tab Navigation

- `Cmd-c`: (`Alt-c`) create new tab
- `Cmd-q`: (`Alt-q`) close tab and hide buffers
- `Cmd-z`: (`Alt-z`) go to previous tab
- `Cmd-x`: (`Alt-x`) go to next tab
- `Cmd-[1-9]`: (`Alt-[1-9]`) select tab by number

#### Buffer/File Navigation

- `,z`: cycle back through previously opened buffers
- `,x`: cycle forward through previously accessed buffers
- `,TAB`: toggle between last two opened buffers
- `,b`: CtrlP buffer selector great for jumping to a file you already have open
- `,,b`: opens buffer selector containing recently opened files
- `,n`: opens file explorer in directory vim was started in
- `,m`: shows current file in file explorer
- `,q`: closes buffer (`:bd!`)
- `,Q`: completely closes Vim without saving any buffers (`:qa!`)
- `,Z`: completely closes Vim saving all buffers (`:xa`)
- `,S`: saves all buffers (`:wa`)

#### Code/Search Navigation

- `Ctrl-o`: Old cursor position this is a standard mapping but very useful, so
  included here
- `Ctrl-i`: opposite of Ctrl-O (again, this is standard)
- `,gf`: same as vim normal gf (go to file)
- `,ag`: Grep command line using Silver Searcher and Fzf
- `,ad`: search for term in directory of current file
- `,aw`: Search the current word under the cursor
- `,hl`: toggle search highlight on and off
- `Ctrl-x Ctrl-o`: vim omnicompletion
- `//`: clear the search
- `,mc`: mark this word for MultiCursor (like sublime). Use `Ctrl-n`: (next),
  `Ctrl-p`: (prev), `Ctrl-x`(skip) to add more cursors, then do normal vim things
  like edit the word.
- `,jx`: EasyMotion Jump to character x
- Spacebar Sneak type two characters to move there in a line. Kind of like vim's
  `f`: but more accurate.
- `:Gsearch foo`: global search, then do your normal `%s/search/replace/g` and
  follow up with `:Greplace` to replace across all files. When done use `:wall`:
  to write all the files.

#### Git Plugin

- `:Git`: Git status with [Fugitive](https://github.com/tpope/vim-fugitive)
- `:Gclog`: view logs or load previous versions of same file
- `:Git push/pull`: repo management
- `]c`, `[c`: jump to next/previous modification

#### Ctag Management

- `:MakeTags`: build tag library for project
- `,gt`: search for tag under cursor and bring up file
- `,gi`: go to next tag in history
- `,go`: go back one tag in history

### Spell Check

- `,W`: Toggle spell checker
- `]s`,`[s`: jump to next/previous spelling error
- `z=`: get spelling suggestion
- `zg`, `zug`: add/remove word from spellfile

#### Conquer of Completion

- `gd`: go to tag definition
- `gy`: go to type definition
- `gi`: go to next implementation of tag
- `gr`: pull up reference preview of tag
- `,rn`: symbol renaming
- `,f`: format selected code
- `:Format`: format code in current buffer
- `:Fold`: fold code in current buffer
- `:OR`: organizes imports of current buffer
- `:Ctrl-f`: scroll up in popup window `:Ctrl-b`: scroll down in popup window
- `,e`: pull up diagnostic window
- `[g`,`]g`: navigate up/down diagnostic window
- `,qf`: autocorrect line error
- `H`,`K` : pull up documentation for item under cursor
- `Ctrl-Space`: Trigger autocompletion

### Snippets

- `C-l`: Trigger snippet expand
- `C-j`: Expand or Jump to next placeholder
- `C-k`: Jump to previous placeholder
- `,x`: Convert visual selected code to a snippet
- `<tab>`: triggers completion, confirmation, expand and jump

#### Asynchronous Recorder

- `q <letter>`: start recording all activity
- `q`: stop recording
- `@ <letter>`: replay activity stored in letter

#### Handy Autocompletions

- `Ctrl-x Ctrl-n`: multiword/tag completion
- `Ctrl-x Ctrl-f`: filename completion
- `Ctrl-x Ctrl-o`: omnicompletion

#### Xdebug

See more about setting up the
[.vimrc.local](https://gitlab.com/odysseyalive/lampreadys#vim-vdebug-settings) file.

- `<F5>`: start/run (to next breakpoint/end of script)
- `<F2>`: step over
- `<F3>`: step into
- `<F4>`: step out
- `<F6>`: stop debugging (kills script)
- `<F7>`: detach script from debugger
- `<F9>`: run to cursor
- `<F10>`: toggle line breakpoint
- `<F11>`: show context variables (e.g. after "eval")
- `<F12>`: evaluate variable under cursor
- `:Breakpoint <type> <args>`: set a breakpoint of any type (see :help VdebugBreakpoints)
- `:BreakpointRemove *`: remove all breakpoints
- `:VdebugEval <code>`: evaluate some code and display the result
- `,e`: evaluate the expression under visual highlight and display the
  result

#### Better keystrokes for common editing commands

- `S#` `S"` `S'` `S]` `S)` `S}`: to surround a visual selection with these
  common wrappers
- `ci'`, `ci"`, `ci]`, `ci)`: (`Alt` Linux) changes content inside those
  surrounding marks. You don't have to be inside them
- `,.`: to go to last edit location (same as `'.`) because the apostrophe is
  hard on the pinky
- `,ci`: to change inside any set of quotes/brackets/etc

#### Misc

- `,,c`: search for and execute commands
- `Ctrl-p`: after pasting Use `p`: to paste and `Ctrl-p`: to cycle through
  previous pastes.
- `,yw`: yank a word from anywhere within the word (so you don't have to go to
  the beginning of it)
- `,ow`: overwrite a word with whatever is in your yank buffer you can be
  anywhere on the word. saves having to visually select it
- `,w`: strip trailing whitespaces
- `,hi`: show current Highlight group. if you don't like the color of something,
  use this, then use `hi! link [groupname] [anothergroupname]`: in your
  vimrc.after to remap the color. You can see available colors using `:hi`
- `:Wrap`: wrap long lines (e.g. when editing markdown files)
- `,;;`, `gcc`: un/comment selected lines in visual mode thanks to [tComment](https://github.com/tomtom/tcomment_vim)
- `,o`,`,p`: start a new or restore an existing Vim session thanks to [obsession](https://github.com/tpope/vim-obsession)
- `zf`,`zo`,`zc`,`za`,`zr`,`zm`,`zd`: creates, opens, closes, toggles, toggles
  all, collapses all and deletes code folds
- `,W`,`z=`,`]s`,`[s`: toggle spell check, word suggestion and jump
  forward/backward to next misspelled word
- `:%!js-beautify`: run external comands on file within Vim, like js-beautify

#### Vim Dev

- `,vc`: (Vim Command) copies the command under your cursor and executes it in
  vim. Great for testing single line changes to vimrc.
- `,vr`: (Vim Reload) source current file as a vim file

# Vim-style split navigation
keybind = ctrl+shift+h=goto_split:left
keybind = ctrl+shift+j=goto_split:bottom
keybind = ctrl+shift+k=goto_split:top
keybind = ctrl+shift+l=goto_split:right

# Leader key sequences (like tmux/vim)
keybind = ctrl+a>c=new_tab
keybind = ctrl+a>v=new_split:right
keybind = ctrl+a>s=new_split:down
```

[Return to top](#yadrlite)

# AeroSpace

AeroSpace is an i3-like tiling window manager for macOS. Unlike yabai, it doesn't require disabling System Integrity Protection (SIP), making it safer and easier to set up. The YADRLite configuration uses vim-style keybindings with Alt as the modifier key.

Requirements: [AeroSpace](https://github.com/nikitabobko/AeroSpace)

[Return to top](#yadrlite)

## AeroSpace Shortcuts

AeroSpace uses `Alt` (Option) as the primary modifier. After installation, grant Accessibility permissions when prompted (System Settings > Privacy & Security > Accessibility).

#### Window Focus

- `Alt+h`: Focus window to the left
- `Alt+j`: Focus window below
- `Alt+k`: Focus window above
- `Alt+l`: Focus window to the right

#### Window Movement

- `Alt+Shift+h`: Move window left
- `Alt+Shift+j`: Move window down
- `Alt+Shift+k`: Move window up
- `Alt+Shift+l`: Move window right

#### Workspaces

- `Alt+1` through `Alt+9`: Switch to workspace 1-9
- `Alt+Shift+1` through `Alt+Shift+9`: Move window to workspace 1-9

#### Layout Controls

- `Alt+/`: Toggle between horizontal and vertical tiling
- `Alt+,`: Toggle accordion layout
- `Alt+f`: Toggle fullscreen
- `Alt+Shift+f`: Toggle floating/tiling mode

#### Splits

- `Alt+\`: Split horizontally
- `Alt+Shift+\`: Split vertically

#### Resize

- `Alt+-`: Shrink window
- `Alt+=`: Expand window

#### Theme

- `Alt+t`: Toggle theme (SeaShells dark/light)

#### Service Mode

- `Alt+Shift+;`: Enter service mode
  - `Esc`: Reload config and return to main mode
  - `r`: Flatten workspace tree
  - `Backspace`: Close all windows except current

#### Float Rules

The following applications open as floating windows by default:

- Finder
- System Settings/Preferences
- Calculator
- Preview

## Mixed Layouts (Accordion + Tiles)

AeroSpace uses a tree structure where each container can have its own layout. This is useful for widescreen monitors where you want different layouts on each side of the screen.

**Example: Accordion on the left, single window on the right**

```
┌─────────────────┬─────────────────┐
│   [Accordion]   │                 │
│   ┌─────────┐   │    Single       │
│   │ Window1 │   │    Window       │
│   │ Window2 │   │    (tiles)      │
│   │ Window3 │   │                 │
│   └─────────┘   │                 │
└─────────────────┴─────────────────┘
```

**How to set it up:**

1. Open two windows - they'll tile horizontally by default
2. Focus the left window
3. Press `Alt+,` to switch that side to accordion
4. Open more windows while focused on the left - they stack in the accordion
5. The right side remains unaffected (tiles layout)

**Useful keybindings for mixed layouts:**

- `Alt+,`: Toggle accordion layout (affects only the focused container)
- `Alt+/`: Toggle between horizontal and vertical tiling

Each container maintains its own layout independently, so changing one side doesn't affect the other.

Configuration location: `~/.config/aerospace/aerospace.toml`

[Return to top](#yadrlite)

# Hyprland

Hyprland is a dynamic tiling Wayland compositor used by [Omarchy](https://omarchy.org), an opinionated Arch Linux distribution. Everything in Omarchy happens via the keyboard. View all keybindings with `Super + K` and customize them in `~/.config/hypr/bindings.conf`.

Requirements: [Hyprland](https://hyprland.org/), [Omarchy](https://omarchy.org/)

[Return to top](#yadrlite)

## Hyprland Shortcuts

Hyprland uses `Super` (Windows/Meta key) as the primary modifier. All bindings can be customized in `~/.config/hypr/bindings.conf`.

#### Essential

- `Super+K`: Show all keyboard bindings
- `Super+Space`: Application launcher
- `Super+Alt+Space`: Omarchy menu
- `Super+Return`: Open terminal
- `Super+Shift+B`: Open browser

#### Window Management

- `Super+W`: Close active window
- `Ctrl+Alt+Delete`: Close all windows
- `Super+T`: Toggle tiling/floating mode
- `Super+O`: Pop window into sticky floating state
- `Super+F`: Toggle fullscreen
- `Super+Alt+F`: Full width mode
- `Super+J`: Toggle stacking windows horizontally/vertically

#### Window Focus

- `Super+Arrow`: Move focus directionally (up/down/left/right)
- `Super+Shift+Arrow`: Swap windows directionally

#### Window Resize

- `Super+=`: Grow window right
- `Super+-`: Grow window left
- `Super+Shift+=`: Grow window down
- `Super+Shift+-`: Grow window up

#### Workspaces

- `Super+1/2/3/4`: Jump to workspace 1-4
- `Super+Tab`: Next workspace
- `Super+Shift+Tab`: Previous workspace
- `Super+Ctrl+Tab`: Return to former workspace
- `Super+Shift+1/2/3/4`: Move window to workspace 1-4

#### Scratchpad

- `Super+S`: Show scratchpad overlay
- `Super+Alt+S`: Move window to scratchpad

#### Window Grouping

- `Super+G`: Toggle grouping
- `Super+Alt+G`: Remove from group
- `Super+Alt+Tab`: Cycle within group
- `Super+Alt+1/2/3/4`: Jump to specific window in group

#### Screen Capture

- `Print`: Screenshot with editing
- `Shift+Print`: Screenshot to clipboard
- `Alt+Print`: Screen recording (press again to stop)
- `Super+Print`: Color picker

#### Utilities

- `Super+Ctrl+E`: Emoji picker
- `Super+Ctrl+Space`: Rotate background images

#### Application Launchers (Super+Shift+Key)

- `Super+Shift+Return`: Terminal
- `Super+Shift+B`: Browser
- `Super+Shift+F`: File manager
- `Super+Shift+N`: Neovim

#### Input Configuration

Customize keyboard repeat rate and trackpad settings in `~/.config/hypr/input.conf`.

Configuration location: `~/.config/hypr/bindings.conf`

[Return to top](#yadrlite)

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

# Credits

Maintained with [Claude Enforcer](https://github.com/odysseyalive/claude-enforcer)
