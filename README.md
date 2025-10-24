# YADRLite

This repository is a collection of tools that I use in my workflow.

Credit:
This dotfile repo is heavily based on [YADR](https://github.com/skwp/dotfiles), a solid
dotfile package created by Yan Pritzker and worked on by several others. This is not
intended to be a fork of YADR. At the same time, I take no credit here, either. This
is just a personal collection of settings from YADR trimmed down considerably for speed
and portability.

Check out the original repo, [YADR](https://github.com/skwp/dotfiles), especially if you
work in Python or Ruby.

**Also, check out LazyVIM!** Recently, I loaded a large project into VSCode, and the editor
started stuttering to an unusable state. As a recent convert from Emacs, I had spent
three days setting the thing up and making friends with it. VSCode was a real
disappointment.

Initially, I found myself disenchanted with broken Emacs plugins. Finally, I was tired of
trying to fix them and just moved on. Then, after all the pain, I stumbled onto [LazyVIM](https://www.lazyvim.org/).
With this package, I can't tell you how amazing NeoVIM is as an IDE. So please do yourself
a favor and check it out.
Under the workstation directory, you'll find an example of my setup using great plugins like
Grammarly and VSCode-php-debug. IMHO, the neovim dap interface is much, much nicer than VSCode's
implementation. For one thing, you can actually read the variable scope.

index: [Emacs](#emacs), [Tmux](#tmux), [Vim](#vim), [LazyVim](#lazyvim), [Kitty](#kitty), and [Ranger](#ranger)

# Dotfiles Installation

Requirements: [git](https://git-scm.com/downloads)

```bash
bash -c "`curl -fsSL https://raw.githubusercontent.com/odysseyalive/dotfiles/master/setup`"
```

#### Install commonly used tools

```bash
bash ~/.yadrlite/setup tools
```

#### Update

```bash
bash ~/.yadrlite/setup update
```

#### Uninstall and Restore to Prior Configuration

```bash
bash ~/.yadrlite/setup remove
```

#### To install this Emacs configuration without the rest of YADRLite

```bash
curl https://raw.githubusercontent.com/odysseyalive/dotfiles/master/emacs.d/emacs.init > ~/.emacs
```

# My Workstation

### Arch workstation

Endeavorous

```bash
curl https://raw.githubusercontent.com/odysseyalive/dotfiles/master/workstation/endeavouros-mercury-neo > ~/install
```

Omarchy

```bash
curl https://raw.githubusercontent.com/odysseyalive/dotfiles/master/workstation/omarchy-latest > ~/install
```

Run as user...

```bash
bash install
```

# What's Included

Workstation shortcuts have been pulled over from YADR, so those working with YADR should
feel right at home. There are a few minor changes and additions. However, a LOT may
be missing. This package is geared towards PHP development on local staging
environments and setting up shop on web servers for emergencies with limited permissions.
The idea is here that the package should be easy to remove while restoring the system to
prior user configurations.

As with YADR, most of the key mapping not mentioned below can be found in the
settings directory for reference. This package is ready for deployment on most
Linux distros and MacOS.

**Vim / Tmux**\
![screenshot](https://github.com/odysseyalive/dotfiles/raw/master/assets/vim-tmux.jpeg?raw=true)

**Emacs**\
![screenshot](https://github.com/odysseyalive/dotfiles/raw/master/assets/emacs.jpeg?raw=true)

# Emacs

Requirements: [Emacs 27.2+ w/ Lua support](https://www.gnu.org/software/emacs/)

Suggested: [Silver Searcher](https://github.com/ggreer/the_silver_searcher)

Emacs has a huge performance advantage over Vi. This configuration includes
the [Evil](https://www.emacswiki.org/emacs/Evil) package, which emulates Vim's
modal functionality. In Evil mode Emacs shares keybindings familiar to the YADR
package. Switching between Emacs and Evil mode is done by C-z. There is inline
documentation with live hints for ease of discovery. After entering Emacs, just
hit the leader key (,) to get started. Hit the leader key twice for a fuzzy
command search.

NOTE: I have some personal concerns about Emacs 28's performance. However, I'm
excited about the new developments in 29. Therefore, I would strongly suggest
sticking with version 27 for now. If your distro doesn't have it, there are
compilation examples in each workstation script.

[Return to top](#yadrlite)

## Emacs Shortcuts

The [General](https://github.com/noctuid/general.el) plugin has been included
with this Emacs configuration. There is no need to hold the comma down (the
leader key borrowed from YADR) before pressing the next key. The following
shortcuts can be typed concurrently in the order presented, which is a friendly
alternative to the Emacs Pinky.

#### Single Key Shortcuts

- `,,`: Search for a command, M-X equivalent
- `,;`: Comment or uncomment code blocks
- `,[`: Contract vertically split Window
- `,]`: Expand vertically split Window
- `,-`: Shrink horizontally split window
- `,=`: Grow hoizontally split window
- `,e`: Toggle Error List
- `,h`: View Dashboard
- `,jx`: Jump to character x
- `,m`: Browse current directory wth Ranger
- `,n`: Browse current directory with Nerdtree
- `,q`: Quit current buffer m
- `:q`: or `:q!` Quit current buffer, Note: all standard VIM controls apply
- `,Q`: Close Emacs
- `,x`: Cycle to next buffer
- `,z`: Cycle to previous buffer
- `,U`: Update All Packages
- `,/`: Search current project

#### Emacs Applications

- `,ai`: Open IRC
- `,at`: Open a terminal
- `,am`: Open Mu4e for Email
- `,aw`: Browse the web from Emacs

#### Buffer Shortcuts

- `,bb`: List/Create buffers
- `,bc`: Copy File Path
- `,bf`: Open File
- `,bn`: Craete New Buffer
- `,bp`: Find File in Project
- `,bq`: Quit buffer
- `,br`: Refresh buffer
- `,bs`: Save buffer
- `:w`: Save buffer, VIM equivalent

#### Dap-Mode Shortcuts: e

- `,da`: Delete all breakpoints
- `,db`: Toggles breakpoint on current line
- `,dc`: Continue to next breakpoint
- `,dd`: Start debugger
- `,di`: Step into
- `,dl`: List all breakpoints
- `,dn`: Next line
- `,do`: Step out
- `,dp`: Install vscode-php-debug plugin
- `,dv`: View context
- `,dx`: Stop Debugger

#### Find

- `,fd`: Find Definitions to hovered function
- `,fD`: Find in Current Buffer's Directory Recursively
- `,ff`: Resume Last Find
- `,fi`: Find Implementation
- `,fm`: Show quick menu of file definitions
- `,fp`: Find file in project
- `,fr`: Find References to hovered function
- `,fs`: Find Documentation

#### Grammer Shortcuts

- `,ga`: See Antonyms
- `,gs`: View spelling errors
- `,gc`: Correct spelling of word under cursor
- `,gd`: See definition of word under cursor
- `,gd`: See related words for word under cursor
- `,gt`: See synonyms (thesaurus) of word under cursor

#### Line Shortcuts

- `,la`: Artist mode, draw on screen with cursor
- `,lc`: Toggle HTML color codes
- `,le`: Encrypt selection
- `,ld`: Decrypt selection
- `,li`: Toggle aggressive indentation
- `,ln`: Toggle line numbers
- `,lm`: Toggle Line/Character mode
- `,lr`: Toggle relative line number display
- `,ls`: Delete trailing whitespaces
- `,lt`: Toggle PHP/Web mode highlighting
- `,lw`: Toggle line wrapping

#### Org-Mode Shortcuts

- `,oo`: Agenda View, be sure to adjust position wth org-agenda-file-to-front
- `,oa`: Activate current line
- `,od`: Schedule deadline
- `,oc`: Recompute clock
- `,ok`: Schedule task
- `,og`: Set tag
- `,ol`: Display clocks
- `,oj`: Jump to running task
- `,ot`: Start task
- `,or`: Show report
- `,os`: Stop task
- `,ox`: Cancel clock

#### Project Shortcuts

- `,p[`: Go to next change in file
- `,p]`: Go to previous change in file
- `,pd`: Search in directory
- `,ph`: History of buffer
- `,pf`: Fuzzy file finder (CtrlP)
- `,pp`: Resume previous search/filter
- `,pr`: Open recent project
- `,ps`: Search in current project
- `,pm`: Git status with Magit
- `,pw`: Swoop

#### Lisp Debugging Shortcuts

- `,sa`: Evaluate statement
- `,sb`: Evaluate buffer
- `,sc`: Evaluate function
- `,se`: Evaluate and print statement
- `,sf`: Set breakpoint on function
- `,sl`: Load e-List file
- `,sp`: Compile File
- `,ss`: Open Lisp scratch pad
- `,sv`: Set breakpoint on variable
- `,sw`: Remove breakpoint from a variable
- `,sx`: Remove breakpoint from a function

#### Window Shortcuts

- `,wd`: Delete current window
- `,wc`: Delete all other windows except current window
- `,wf`: Toggle Emacs full screen
- `,wh`: Focus on window to left of current one
- `,wj`: Focus on window above current one
- `,wk`: Focus on window below current one
- `,wl`: Focus on window to right of current one
- `,wH`: Move current window left
- `,wJ`: Move current window up
- `,wK`: Move current window down
- `,wL`: Move current window right
- `,wn`: Switch screens
- `,wo`: Focus on next window
- `,wp`: Pop out current window to new screen
- `,ws`: Split window horizontally
- `,wv`: Split window vertically
- `,wx`: Close screen

#### Other Shortcuts

- `<ctrl>-z`: Toggles Emacs/Vim Mode
- `<ctrl>-x t`: Tab Controls

[Return to top](#yadrlite)

# Tmux

Requirements: [Tmux](https://github.com/tmux/tmux/wiki/Installing)

[Return to top](#yadrlite)

## Tmux Shortcuts

The leader key Ctrl-a can be followed by the next key concurrently without
holding them all down at once. Use `tmux ls` to list the current open sessions
and `tmux a -t <session_no>` to join them. Use
`tmux kill-session -t <session_no>` to remove one.

#### Session Management

- `Ctrl-a d`: Detach from current session and close Tmux. Run `tmux attach`
  to resume later.
- `Ctrl-a ^D`: Alternative detach command (keep finger on Ctrl)
- `Ctrl-a Ctrl-s`: Save current session with tmux-resurrect
- `Ctrl-a Ctrl-r`: Restore saved session with tmux-resurrect

#### Window Management

- `Ctrl-a c`: Create new window in current directory
- `Ctrl-a Ctrl-a`: Switch to last window
- `Ctrl-a a`: Send prefix to nested tmux session
- `Ctrl-a &`: Force close window

#### Pane Navigation

- `Ctrl-h`: Focus on pane left of the current one (vim-tmux-navigator)
- `Ctrl-j`: Focus on pane below the current one (vim-tmux-navigator)
- `Ctrl-k`: Focus on pane above the current one (vim-tmux-navigator)
- `Ctrl-l`: Focus on pane right of the current one (vim-tmux-navigator)

#### Pane Creation

- `Ctrl-a v`: Create vertical split (50% width) in current directory
- `Ctrl-a ^V`: Alternative vertical split command
- `Ctrl-a s`: Create horizontal split (50% height) in current directory
- `Ctrl-a ^S`: Alternative horizontal split command

#### Pane Resizing

- `Ctrl-a H`: Resize pane left by 5 columns (repeatable)
- `Ctrl-a J`: Resize pane down by 5 rows (repeatable)
- `Ctrl-a K`: Resize pane up by 5 rows (repeatable)
- `Ctrl-a L`: Resize pane right by 5 columns (repeatable)

#### Pane Synchronization

- `Ctrl-a e`: Enable synchronize-panes (send input to all panes)
- `Ctrl-a E`: Disable synchronize-panes

#### Copy Mode

- `Ctrl-a [`: Enter copy mode (Vim-like navigation)
- In copy mode:
  - `h,j,k,l`: Move cursor
  - `v`: Begin selection
  - `Ctrl-v`: Begin rectangular selection
  - `y`: Copy selection and exit copy mode
  - `Enter`: Exit copy mode

#### Configuration

- `Ctrl-a r`: Reload tmux configuration file
- `Ctrl-a Ctrl-l`: Clear screen (alternative to clear command)

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

# LazyVim

LazyVim is a modern Neovim distribution that provides a blazing fast IDE experience with sensible defaults. This configuration builds on LazyVim's foundation with PHP development optimizations and carefully selected plugins for web development workflows.

```bash
mkdir -p ~/.config/nvim && rsync -azhLP ~/.yadrlite/workstation/lazyvim/ ~/.config/nvim
```

Requirements: [Neovim 0.9+](https://neovim.io/), [Git](https://git-scm.com/), [Node.js](https://nodejs.org/)

Plugin Requirements: [Ripgrep](https://github.com/BurntSushi/ripgrep), [fd](https://github.com/sharkdp/fd), [PHP CS Fixer](https://cs.symfony.com/), [Intelephense](https://intelephense.com/)

[Return to top](#yadrlite)

## LazyVim Shortcuts

LazyVim uses the comma (`,`) as the leader key, consistent with the YADR package. Most shortcuts are discoverable through the which-key popup that appears after pressing the leader key.

#### Core Navigation

- `,`: Opens which-key menu (shows all available shortcuts)
- `,,`: Command palette (equivalent to `:` in vim)
- `,r`: Resume last fuzzy finder search
- `Ctrl-h,j,k,l`: Navigate between splits and tmux panes
- `Ctrl-/`: Toggle terminal
- `<S-h>`,`<S-l>`: Switch between buffers

#### File Management

- `,ff`: Find files in project
- `,fr`: Recent files
- `,fb`: Browse file system
- `,fg`: Live grep in project
- `,fw`: Find word under cursor
- `,fc`: Find command
- `,fh`: Help tags
- `,fm`: File manager (neo-tree)

#### Buffer Operations

- `,bd`: Delete buffer
- `,bD`: Delete buffer and window
- `,bl`: Delete buffers to the left
- `,br`: Delete buffers to the right
- `,bo`: Delete other buffers
- `,bp`: Toggle pin buffer

#### LSP and Code Actions

- `gd`: Go to definition
- `gr`: Go to references
- `gi`: Go to implementation
- `K`: Show hover documentation
- `,ca`: Code actions
- `,cr`: Rename symbol
- `,cf`: Format document
- `,cd`: Line diagnostics
- `]d`,`[d`: Next/previous diagnostic

#### Git Integration

- `,gg`: LazyGit
- `,gb`: Git blame line
- `,gf`: LazyGit current file history
- `,gl`: Git log
- `,gL`: Git log (current file)
- `]h`,`[h`: Next/previous hunk
- `,ghs`: Stage hunk
- `,ghr`: Reset hunk
- `,ghp`: Preview hunk

#### Debug (DAP)

- `,db`: Toggle breakpoint
- `,dB`: Breakpoint condition
- `,dc`: Continue
- `,dC`: Run to cursor
- `,dg`: Go to line (no execute)
- `,di`: Step into
- `,dj`: Down in stacktrace
- `,dk`: Up in stacktrace
- `,dl`: Run last
- `,do`: Step out
- `,dO`: Step over
- `,dp`: Pause
- `,dr`: Toggle REPL
- `,ds`: Session
- `,dt`: Terminate
- `,dw`: Widgets

#### Window Management

- `,ww`: Other window
- `,wd`: Delete window
- `,w-`: Split window below
- `,w|`: Split window right
- `,wm`: Maximize toggle

#### Search and Replace

- `,sr`: Replace in files
- `,sR`: Replace in files (with confirmation)
- `,sw`: Search word under cursor
- `,sW`: Search word under cursor (exact match)

#### Project and Session

- `,fp`: Find files in config
- `,ft`: Terminal (root dir)
- `,fT`: Terminal (cwd)
- `,qq`: Quit all
- `,qs`: Session save
- `,qr`: Session restore

#### Plugin Management

- `,l`: Open Lazy plugin manager
- `,cm`: Mason (manage LSP servers, formatters, etc.)
- `,ci`: Lazy install
- `,cs`: Lazy sync
- `,cu`: Lazy update
- `,cx`: Lazy extras

#### AI and Copilot

- `,aa`: CopilotChat
- `,ae`: CopilotChat explain
- `,af`: CopilotChat fix
- `,ao`: CopilotChat optimize
- `,ad`: CopilotChat docs
- `,at`: CopilotChat tests
- `Tab`: Accept Copilot suggestion
- `Ctrl-]`: Dismiss Copilot suggestion

#### Table Mode (for Markdown/Org)

- `,tm`: Toggle table mode
- `,tt`: Tableize (convert CSV to table)
- `,tr`: Realign table
- `,tdd`: Delete table row
- `,tdc`: Delete table column

#### Org Mode

- `,oa`: Org agenda
- `,oc`: Org capture
- `,oe`: Org export
- `,oi`: Org clock in
- `,oo`: Org clock out
- `,ot`: Org todo

[Return to top](#yadrlite)

# Kitty

Kitty is a full featured GPU-accelerated terminal. It's faster.

Requirements: [Kitty](https://sw.kovidgoyal.net/kitty/)

[Return to top](#yadrlite)

## Kitty Shortcuts

#### History

- `Ctrl+Shift+h` Enter history
- `Ctrl+Shift+p, v` Enter history in VIM mode

#### Tabs

- `ctrl+shift+t`: Creates new tab
- `ctrl+shift+q`: Closes tab
- `ctrl+shift+right`: Next tab
- `ctrl+shift+left`: Last tab
- `ctrl+shift+.`: Move tab forward
- `ctrl+shift+,`: Move tab backward

#### Windows

- `ctrl+shift+enter`: New window
- `ctrl+shift+w`: Close window
- `ctrl+shift+n`: Create new OS window
- `ctrl+shift+]`: Next window
- `ctrl+shift+[`: Last window
- `ctrl+shift+l`: Cycle through layout
- `ctrl+shift+f`: Shift window to prominent spot
- `ctrl+shift+b`: Shift window to least prominent spot
- `ctrl+shift+F2`: Create a new iteration of the last edited window

#### Display Options

- `Ctrl+Shift+Equal`: Inscrease Font Size
- `Ctrl+Shift+minus`: Decrease Font Size
- `Ctrl+Shift+backspace`: Restore Font Size
- `Ctrl+Shift+O`: select light and dark themes
- `Ctrl+Shift+L`: Select light theme
- `Ctrl+Shift+D`: Select dark theme

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
