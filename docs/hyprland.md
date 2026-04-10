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

