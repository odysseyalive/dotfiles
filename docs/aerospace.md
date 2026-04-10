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

