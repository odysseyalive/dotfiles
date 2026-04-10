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

