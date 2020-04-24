# YADRLite #

Credit:
---
This dotfile repo is heavily based on [YADR](https://github.com/skwp/dotfiles) which is a solid dotfile package created by Yan Pritzker and worked on by several others.  This is not intended to be a fork of YADR.  At the same time, I take no credit here either.  This is just a personal collection of settings from YADR trimmed down considerably for the sake of speed and portability.

Check out the original repo, [YADR](https://github.com/skwp/dotfiles) especially if you work in Python or Ruby.


Installation:
---

```bash
bash -c "`curl -fsSL https://raw.githubusercontent.com/bridgesense/dotfiles/master/setup`"
```

#### Update:

```bash
bash ~/.yadrlite/setup update
```

#### Uninstall and Restore to Prior Configuration:

```bash
bash ~/.yadrlite/setup remove
```

#### To install this Emacs configuration without the rest of YADRLite:

```bash
curl https://raw.githubusercontent.com/bridgesense/dotfiles/master/emacs.init > ~/.emacs
```

#### On MacOS [Homebrew](https://brew.sh) has a nice [Cask](https://emacsformacosx.com) package for a GUI version of Emacs

```bash
brew cask install emacs
```

What's Included:
---
Shortcuts have been pulled over from YADR, so those working with YADR should feel right at home.  There are a few minor changes and additions.  A LOT may be missing.  This package is geared towards PHP development on local staging environments and setting up shop on web servers for emergencies where there may be limited permissions.  The idea is here that the package should be easy to remove while restoring the system to prior user configurations.

As with YADR, most of the key mapping not mentioned below can be found in the settings directory for reference.  This package is ready for deployment on most Linux distros and MacOS.

Vim:
---
![screenshot](https://www.bridgesense.com/images/dotfiles/vim-style.jpg)

Emacs:
---
![screenshot](https://www.bridgesense.com/images/dotfiles/emacs-style.jpg)

Emacs 24.4 or higher is preferred.

Emacs has a huge performance advantage over Vi.  This configuration includes the [Evil](https://www.emacswiki.org/emacs/Evil) package which emulates Vim's modal functionality.  In Evil mode Emacs shares keybindings familiar to the YADR package.  Switching between Emacs and Evil mode is done by C-z. There is inline documentation with live hints for ease of discovery.  After entering Emacs just hit the leader key (,) to get started.  Hit the leader key twice for a fuzzy command search.


Emacs Shortcuts
---
The [General](https://github.com/noctuid/general.el) plugin has been included with this Emacs configuration. There is no need to hold the comma down (the leader key borrowed from YADR) before pressing the next key. The following shortcuts can be typed concurrantly in the order presented which is a nice alternative to the Emacs Pinky.

#### Single Key Shortcuts:
* `,,`: Search for a command, M-X equivalentThe f
* `,;`: Comment or uncomment code blocks
* `,[`: Contract vertically split Window
* `,]`: Expand vertically split Window
* `,-`: Shrink horizontally split window
* `,=`: Grow hoizontally split window
* `,a`: Search project
* `,d`: Open directory browser, Ranger equivalent
* `,e`: Toggle Error List
* `,jx`: Jump to character x
* `,n`: Browse current directory, Nerdtree equivalent
* `,q`: Kill current buffer
* `:q`: or `:q!` Kill current buffer, Note: all standard VIM controls apply
* `,Q`: Quit Emacs
* `,x`: Cycle to next buffer
* `,z`: Cycle to previous buffer

#### Buffer Shortcuts:
* `,bb`: List/Create buffers
* `,bs`: Save buffer
* `:w`: Save buffer, VIM equivalent
* `,br`: Refresh buffer

#### Window Shortcuts:
* `,wd`: Delete windown
* `,wc`: Delete all other windows except current window
* `,wf`: Toggle Emacs full screen
* `,wh`: Focus on window to left of current one
* `,wj`: Focus on window above current one
* `,wk`: Focus on window below current one
* `,wl`: Focus on window to right of current one
* `,wH`: Move current window left
* `,wJ`: Move current window up
* `,wK`: Move current window down
* `,wL`: Move current window right
* `,wn`: Switch screens
* `,wo`: Focus on next window
* `,wp`: Pop out current window to new screen
* `,ws`: Split window horizontally
* `,wv`: Split window vertically
* `,wx`: Close screen

#### Emacs Applications:
* `,ai`: Open IRC
* `,at`: Open a terminal
* `,am`: Open Gnus for News/Email
* `,ap`: Open a PHP scratch pad or interactive shell, requires Boris
* `,aw`: Browse the web from Emacs

#### Geben (Xdebug) Shortcuts:
* `,gb`: Add Breakpoint to current line
* `,gc`: Clear all breakpoints
* `,gm`: Setup path mapping ([see explanation](https://gitlab.com/bridgesense/lampready#emacs-geben-settings))
* `,go`: Start Geben
* `,gr`: Continue to breakpoint
* `,gv`: View context
* `,gx`: Stop Geben

#### Line Shortcuts:
* `,la`: Artist mode, draw on screen with cursor
* `,lc`: Toggle HTML color codes
* `,le`: Encrypt selection
* `,ld`: Decrypt selection
* `,li`: Toggle aggressive indentation
* `,ln`: Toggle line numbers
* `,lm`: Toggle Line/Character mode
* `,lr`: Toggle relative line number display
* `,ls`: Delete trailing whitespaces
* `,lt`: Toggle PHP/Web mode highlighting
* `,lw`: Toggle line wrapping

#### Emacs Display Options:
* `,Ot`: Toggle Dark/Light Theme

#### Org-Mode Shortcuts:
* `,oo`: Agenda View, be sure to adjust position wth org-agenda-file-to-front
* `,oa`: Activate current line
* `,od`: Schedule deadline
* `,oc`: Recompute clock
* `,ok`: Schedule task
* `,og`: Set tag
* `,ol`: Display clocks
* `,oj`: Jump to running task
* `,ot`: Start task
* `,or`: Show report
* `,os`: Stop task
* `,ox`: Cancel clock

#### Project Shortcuts:
* `,pd`: Search in directory
* `,ph`: History of buffer
* `,pf`: Fuzzy file finder (CtrlP)
* `,pP`: Open recent project
* `,pp`: Resume previous search/filter
* `,ps`: Search in current project
* `,pm`: Git status with Magit
* `,pw`: Swoop

#### Word Shortcuts:
* `,Ws`: View spelling errors
* `,Wc`: Correct spelling of word under cursor
* `,Wd`: See definition of word under cursor
* `,Wt`: See synonyms (thesaurus) of word under cursor
* `,Wa`: See antonyms of word under cursor


Tmux Shortcuts
---
The leader key Ctrl-a can be followed by the next key concurrently without holding them all down at once.  Use `tmux ls`to list the current open sessions and `tmux a -t <session_no>` to join them.  Use `tmux kill-session -t <session_no>`: to remove one.

#### General Shortcuts:
* `Ctrl-a d`: Detatch from current session and close Tmux.  Run `tmux attach`: to resume later.
* `Ctrl-a h`: Focus on window left of the current one
* `Ctrl-a j`: Focus on window above the current one
* `Ctrl-a k`: Focus on window below the current one
* `Ctrl-a l`: Focus on window right of the current one
* `Ctrl+H`: Shift current window pane left
* `Ctrl+J`: Shift current window pane up
* `Ctrl+K`: Shift current window pane down
* `Ctrl+L`: Shift current window pane right
* `Ctrl-a s`: Create new window horizontally below current one
* `Ctrl-a v`: Create new window vertically to right of current one
* `Ctrl-a [`: Enter Vim-like normal mode
In visual mode use `h,j,k,l` to move, `v` to change to visual mode and `y` to yank selection.  Press `Enter` to exit mode.


VIM Shortcuts
---

#### Window Navigation:
* `Cmd-[1-]`: (`Alt-[1-9]`) switches to a specific tab number (like iTerm and Chrome) and tabs have been set up to show numbers
* `Ctrl-h,l,j,k`: to move left, right, down, up between splits. This also works between vim and tmux splits thanks to `vim-tmux-navigator`.
* `vv`: vertical split (`Ctrl-w,v`)
* `ss`: horizontal split (`Ctrl-w,s`)
* `,c`: closes window only (`Ctrl-w,c`)
* `,qo`: open quickfix window (this is where output from Grep goes)
* `,qc`: close quickfix
* `CMD-[Up,Down,Left,Right]`: increase hight, decrease height, contract window and expand window
* `=`: make all windows equal size

#### Tab Navigation
* `CMD-c`: (`Alt-c`) create new tab
* `CMD-q`: (`Alt-q`) close tab and hide buffers
* `CMD-z`: (`Alt-z`) go to previous tab
* `CMD-x`: (`Alt-x`) go to next tab
* `CMD-[1-9]`: (`Alt-[1-9]`) select tab by number

#### Buffer/File Navigation:
* `,z`: go to previous buffer (:bp)
* `,x`: go to next buffer (:bn)
* `,t`: CtrlP fuzzy file selector
* `,b`: CtrlP buffer selector great for jumping to a file you already have open
* `,,b`: opens buffer selector containing recently opened files  
* `,n`: show current file in NERDTree
* `,q`: closes buffer without saving (`:bd!`)
* `,Q`: completely closes Vim without saving any buffers (`:qa!`)
* `,Z`: completely closes Vim saving all buffers (`:xa`)
* `,S`: saves all buffers (`:wa`)

#### Code/Search Navigation:
* `Cmd-[jk]`: (`Alt-[jk]` move up and down roughly by functions 
* `Ctrl-o`: Old cursor position this is a standard mapping but very useful, so included here
* `Ctrl-i`: opposite of Ctrl-O (again, this is standard)
* `,gf`: or `Ctrl-f` same as vim normal gf (go to file)
* `,k`: Search the current word under the cursor and show results in quickfix window
* `,K`: Grep the current word up to next exclamation point
* `,hl`: toggle search highlight on and off
* `,gg`: or `,ag` Grep command line, type between quotes. Uses Ag Silver Searcher.
* After searching with `,gg` you can navigate the results with `Ctrl-x` and `Ctrl-z` (or standard vim `:cn`: and `:cp`)
* `,gd`: Grep def (greps for 'def [function name]') when cursor is over the function name
* `,gcf`: Grep Current File to find references to the current file
* `//`: clear the search
* `,jx`: EasyMotion Jump to character x
* `,mc`: mark this word for MultiCursor (like sublime). Use `Ctrl-n`: (next), `Ctrl-p`: (prev), `Ctrl-x`(skip) to add more cursors, then do normal vim things like edit the word.
* `gK`: Opens the documentation for the word under the cursor.
* Spacebar Sneak type two characters to move there in a line. Kind of like vim's `f`: but more accurate.
* `:Gsearch foo`: global search, then do your normal `%s/search/replace/g` and follow up with `:Greplace` to replace across all files. When done use `:wall`: to write all the files.

#### Git Plugin:
* `:GStatus`: Git status with [Fugitive](https://github.com/tpope/vim-fugitive)
* `:GLog`,`:Glog -- %`,`:0Glog`: view logs or load previous versions of same file
* `:Git push/pull`: repo management
* `:Gpush/pull`: repo management
* `]c`, `[c`: jump to next/previous modification

#### Tag Management:
* `:MakeTags`: build tag library for project
* `,gf`: search for tag under cursor and display results
* `,gt`: search for tag under cursor and bring up file
* `,gi`: go to next tag in history
* `,go`: go back one tag in history

#### Asynchronous Recorder:
* `q <letter>`: start recording all activity
* `q`: stop recording
* `@ <letter>`: replay activity stored in letter

#### Handy Autocompletions:
* `Ctrl-x Ctrl-n`: multiword/tag completion
* `Ctrl-x Ctrl-f`: filename completion
* `Ctrl-x Ctrl-o`: omnicompletion

#### Xdebug
See more about setting up the [.vimrc.local](https://gitlab.com/bridgesense/lampready#vim-vdebug-settings) file.
* `<F5>`: start/run (to next breakpoint/end of script)
* `<F2>`: step over
* `<F3>`: step into
* `<F4>`: step out
* `<F6>`: stop debugging (kills script)
* `<F7>`: detach script from debugger
* `<F9>`: run to cursor
* `<F10>`: toggle line breakpoint
* `<F11>`: show context variables (e.g. after "eval")
* `<F12>`: evaluate variable under cursor
* `:Breakpoint <type> <args>`: set a breakpoint of any type (see :help
  VdebugBreakpoints)
* `:VdebugEval <code>`: evaluate some code and display the result
* `,e`: evaluate the expression under visual highlight and display the
  result

#### Better keystrokes for common editing commands:
* Ctrl-Space to autocomplete. Tab for snipmate snippets.
* `,#` `,"` `,'` `,]` `,)` `,}`: to surround a word in these common wrappers. the # does #{ruby interpolation}. works in visual mode (thanks @cj). Normally these are done with something like `ysw#`
* `Cmd-'`, `Cmd-"`, `Cmd-]`, `Cmd-)`: (`Alt` Linux) changes content inside those surrounding marks. You don't have to be inside them 
* `,.`: to go to last edit location (same as `'.`) because the apostrophe is hard on the pinky
* `,ci`: to change inside any set of quotes/brackets/etc

#### Misc:
* `Ctrl-p`: after pasting Use `p`: to paste and `Ctrl-p`: to cycle through previous pastes.
* `,yw`: yank a word from anywhere within the word (so you don't have to go to the beginning of it)
* `,ow`: overwrite a word with whatever is in your yank buffer you can be anywhere on the word. saves having to visually select it
* `,w`: strip trailing whitespaces
* `,hi`: show current Highlight group. if you don't like the color of something, use this, then use `hi! link [groupname] [anothergroupname]`: in your vimrc.after to remap the color. You can see available colors using `:hi`
* `:Wrap`: wrap long lines (e.g. when editing markdown files)
* `Cmd-/`: (`Alt-/`) toggle comments (usually gcc from tComment) 
* `,;`, `gcc`: un/comment selected lines in visual mode
* `,O`: make/overwrite a Vim session
* `zf`,`zo`,`zc`,`za`,`zr`,`zm`,`zd`: creates, opens, closes, toggles, toggles all, collapses all and deletes code folds
* `F7`,`z=`,`]s`,`[s`: spell check, word suggestion and jump to next misspelled word

#### Vim Dev:
* `,vc`: (Vim Command) copies the command under your cursor and executes it in vim. Great for testing single line changes to vimrc.
* `,vr`: (Vim Reload) source current file as a vim file


Bspwm/i3 Commands
---
These commands are part of the sample workstations included in this repo.  This is designed to be used with a "Custom Fedora" installation via the server Net install disk.  If you're interested in checking it out, download the file and review it before installing it on a new system.

### Bspwm Workstation Installation
```bash
# Log in as root on a clean barebones install
curl https://raw.githubusercontent.com/bridgesense/dotfiles/master/bspwm/install-bspwm-workstation > custom-fedora-setup
# The workstation user must already be set up
bash custom-fedora-setup <workstation_username>
```

### i3 Workstation Installation
```bash
# Log in as root on a clean barebones install
curl https://raw.githubusercontent.com/bridgesense/dotfiles/master/i3/install-i3-gaps-workstation > custom-fedora-setup
# The workstation user must already be set up
bash custom-fedora-setup <workstation_username>
```

#### Software Bindings:
* `Super+Return`: Brings up the terminal
* `Super+Space`: Brings up an application menu
* `Super+d`: Brings up Nemo a graphical file manager
* `Super+i`: Brings up htop, a process manager
* `Super+m`: Movie Mode toggles the screen saver and lock system
* `Super+n`: Brings up Ranger, a curses file manager
* `Super+r`: Refreshes screens on all monitors
* `Super+Escape`: Swaps Escape with All Caps Lock

#### Screenshots
* `Print`, `Super+Shift+s`: Brings up flameshot, a OSX style screenshot application
* `Shift+Print`: Brings up flameshot configuration

#### Machine Bindings
* `Super+Shift+r`: Brings up reboot dialog
* `Super+Shift+x`: Brings up shutdown dialog

#### Window Movement Bindings
* `Super+h`: Shift focus to left one window
* `Super+j`: Shift focus down one window
* `Super+k`: Shift focus up one window
* `Super+l`: Shift focus right one window
* `Super+Shift+h`: Move current window to the left
* `Super+Shift+j`: Move current window down one
* `Super+Shift+k`: Move current window up one
* `Super+Shift+l`: Move current window to the right
* `Super+Shift+c`: Move current window to the center (i3 only)

* `Super+q`: Close current window (and program)
* `Super+t`: Toggle Split between horizontal and vertical mode
* `Super+s`: Change window to vertical split mode (i3 only)
* `Super+Shift-s`: Reload sxhkd configurations (Bspwm only)
* `Super+v`: Change window to horizontal split mode (i3 only)
* `Super+b`: Toggle window sticky mode

* `Super+left bracket`: shrink window width (i3 only)
* `Super+right bracket`: grow window width (i3 only)
* `Super+minus`: shrink window height (i3 only)
* `Super+equal`: grow window height (i3 only)

* `Super+Alt+h`: expand window frame left (Bspwm only)
* `Super+Alt+j`: expand window frame down (Bspwm only)
* `Super+Alt+k`: expand window frame up (Bspwm only)
* `Super+Alt+l`: expand window frame right (Bspwm only)

#### Window State Bindings
* `Super+f`: Make window full screen
* `Super+p`: Pop out window, floating mode
* `Super+t`: Snap window into place, tiled (default) mode

#### Workspace Bindings
* `Super+1`: jump to / create Workspace 1
* `Super+2`: jump to / create Workspace 2
* `Super+3`: jump to / create Workspace 3
* `Super+4`: jump to / create Workspace 4
* `Super+5`: jump to / create Workspace 5
* `Super+6`: jump to / create Workspace 6
* `Super+7`: jump to / create Workspace 7
* `Super+8`: jump to / create Workspace 8
* `Super+9`: jump to / create Workspace 9
* `Super+0`: jump to / create Workspace 10
* `Super+Shift+{0-9}`: Move window to workspace 0-9

* `Super+w`: jump to right workspace round robin
* `Super+period`: move workspace to right screen round robin
* `Super+slash`: move window to right screen round robin

* `Super+z`: jump to previous workspace
* `Super+x`: jump to next workspace
* `Super+Tab`: Toggle between last selected workspace

#### Function Keys
* `Super+F1`, `Super+F2`, `Super+F3`, `Super+F4`: Brings up the Alsa Mixer
* `Super+F5`, `Super+F6`: Put computer into hybernation without dialog
* `Super+F7`, `Super+F8`: Brings up Network dialog
* `Super+F9`: Locks current screen
* `Super+F10`: Brings up multiple screen setup
* `Super+F11`: Toggles laptop touchpad on/off
* `Super+F12`: Toggles laptop touchpad on/off


