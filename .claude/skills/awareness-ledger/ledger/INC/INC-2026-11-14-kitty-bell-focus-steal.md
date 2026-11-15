# INC-2026-11-14-kitty-bell-focus-steal

**Status:** resolved
**Tags:** omarchy, hyprland, kitty, focus_on_activate, xdg-activation, bell, window-rules, workstation/omarchy-latest, ~/.config/hypr/hyprland.lua
**Related:** [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC-2026-08-17-omarchy4-conf-to-lua-migration.md)

## What Happened

When a job finished in one kitty window (a Claude Code turn ending, a tmux
bell), focus jumped to that window while the user was typing in another kitty
window on workspace 11.

## Timeline

| Time/Commit | Event |
|-------------|-------|
| 2026-11-15 01:00Z | Peer session tr-dev diagnosed the cause and handed the fix to this repo's session |
| 2026-11-14 (local) | Rule added live to `~/.config/hypr/hyprland.lua`, Hyprland reloaded, bell test run |
| d1641e5 | `workstation/omarchy-latest` appends the same rule on install |

## Root Cause

kitty's `window_alert_on_bell yes` (`~/.config/kitty/kitty.conf:389`) calls
`xdg_activation_v1_activate` on Wayland (kitty `glfw/wl_window.c:1893`).
Omarchy sets `focus_on_activate = true` (`default/hypr/looknfeel.lua:111`), so
Hyprland honours the activation and focuses the bell window. tmux is not
involved.

## Contributing Factors (Swiss Cheese Layers)

1. **kitty bell alert** — requests window attention through xdg-activation,
   which a compositor may treat as a focus request.
2. **Omarchy default `focus_on_activate = true`** — Hyprland grants every
   activation request focus. Omarchy itself opts Telegram out
   (`default/hypr/apps/telegram.lua`), but not terminals.

## Resolution

- Appended to `~/.config/hypr/hyprland.lua` (marker `yadrlite: kitty no focus steal`):
  `o.window("^kitty$", { focus_on_activate = false })` — same helper and
  property as Omarchy's Telegram rule. kitty's bell alert left on.
- `workstation/omarchy-latest` adds the block once (grep on the marker), Lua
  only, then `hyprctl reload`.
- Verified: `hyprctl reload` → `ok`, `hyprctl configerrors` empty. A BEL written
  to the pty of an unfocused kitty window produced `bell>>5640e81cc240` on
  Hyprland's socket2 and the active window stayed on the focused tmux window.
- Limits of the check: `hyprctl getprop <addr> focus_on_activate` returns
  `false` for every window (Chrome too), so it does not distinguish "rule set"
  from "unset"; and no without-rule comparison was run, since that would steal
  the user's focus.

## Lessons Learned

> **"On Omarchy, any app that requests xdg-activation steals focus because `focus_on_activate = true` is the default. Opt the app out per class with `o.window(class, { focus_on_activate = false })` in `~/.config/hypr/hyprland.lua` rather than turning off the app's alert or the global option."**

*— Captured 2026-11-14, source: conversation (diagnosis by peer session tr-dev)*
