# Ghostty

Ghostty is a fast, feature-rich, and cross-platform terminal emulator that uses platform-native UI and GPU acceleration. It supports tabs, splits, and the Kitty graphics protocol for image previews in ranger.

Requirements: [Ghostty](https://ghostty.org/)

[Return to top](#yadrlite)

## Ghostty Configuration

The YADRLite Ghostty configuration uses the SeaShells theme with FiraCode Nerd Font. Key features:

- **Font**: FiraCode Nerd Font 10.5pt
- **Theme**: SeaShells Dark/Light (switchable)
- **macOS Option as Alt**: Enabled (required for AeroSpace keybindings)
- **Kitty Graphics Protocol**: Supported natively (ranger image previews work)
- **GPU Acceleration**: Metal on macOS, OpenGL on Linux

Configuration location: `~/.config/ghostty/config`

## Ghostty Shortcuts

Ghostty supports tabs, splits, and windows with native UI components. All keybindings are customizable via the config file.

#### Tabs

- `Ctrl+Shift+t`: New tab
- `Ctrl+Shift+Left`: Previous tab
- `Ctrl+Shift+Right`: Next tab
- `Ctrl+Shift+w`: Close tab/split
- `Ctrl+1-9`: Go to tab by number (macOS: `Cmd+1-9`)

#### Splits

- `Ctrl+Shift+o`: New split (right)
- `Ctrl+Shift+e`: New split (down)
- `Ctrl+Alt+Arrow`: Navigate to split (up/down/left/right)
- `Ctrl+Shift+z`: Toggle split zoom (fullscreen current split)
- `Ctrl+Shift+=`: Equalize split sizes
- `Ctrl+d`: Close split (macOS)

#### Windows

- `Ctrl+Shift+n`: New window
- `Ctrl+Shift+q`: Quit Ghostty

#### Clipboard & Selection

- `Ctrl+Shift+c`: Copy
- `Ctrl+Shift+v`: Paste
- `Shift+Click`: Select text

#### Font Size

- `Ctrl+Shift+=`: Increase font size
- `Ctrl+Shift+-`: Decrease font size
- `Ctrl+Shift+0`: Reset font size

#### Other

- `Ctrl+Shift+p`: Command palette (Linux 1.2+)
- `Ctrl+Shift+,`: Open config file
- `Ctrl+Shift+r`: Reload config

#### Custom Keybindings

Add to `~/.config/ghostty/config`:

```
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

