# INC-2026-08-17-omarchy4-conf-to-lua-migration

**Status:** resolved
**Tags:** omarchy, omarchy4, hyprland, workstation/omarchy-latest, ~/.config/hypr/input.lua, ~/.config/hypr/bindings.lua, ~/.config/hypr/monitors.lua, ~/.config/omarchy/shell.json, wireplumber, pipewire, xkb, chinuk-wawa, install-scripts
**Related:** []

## What Happened

After updating to Omarchy 4.0 ("quatro"), the user lost many desktop
customizations at once: Caps/Escape swap, the Chinuk Wawa keyboard layout and
its Alt+Shift toggle, the bar position (moved back to top), voice-dictation
keybinding (SUPER+ALT+V), key-repeat/touchpad feel, correct display scaling, and
the audio-suspend workaround. Re-running the `workstation/omarchy-latest` install
script did **not** restore them.

## Timeline

| Time/Commit | Event |
|-------------|-------|
| Omarchy 4.0 update | Hyprland config migrated from `~/.config/hypr/*.conf` to Lua (`*.lua`); old `.conf` files left on disk but inert |
| same update | `/etc/vconsole.conf` reset to `us`; system-level XKB layout `/usr/share/X11/xkb/symbols/custom` wiped (user-level `~/.config/xkb/symbols/custom` survived) |
| WirePlumber 0.5.x | Lua config API removed; `~/.config/wireplumber/main.lua.d/*.lua` (`alsa_monitor.rules`) silently ignored |
| 2026-08-17 | Diagnosed and fixed live configs; modernized install scripts in dotfiles and chinuk-wawa-custom-keyboard repos |

## Root Cause

Omarchy 4 changed Hyprland's config format from `.conf` (sourced `.conf` files)
to Lua modules loaded via `require()` in `~/.config/hypr/hyprland.lua`. The
user's `omarchy-latest` install script and the `chinuk-wawa-custom-keyboard`
`hyprland-install.sh` both wrote settings into the now-inert `.conf` files
(`input.conf`, `bindings.conf`, `hyprland.conf`), so nothing they configured took
effect. Independently, WirePlumber 0.5 dropped the Lua config API, killing the
audio-suspend workaround that lived in `main.lua.d/50-alsa-config.lua`.

## Contributing Factors (Swiss Cheese Layers)

1. **Upstream format migration** — Omarchy 4 moved `.conf` → Lua with no
   automatic port of user files in the old locations.
2. **Install scripts pinned to the old format** — the scripts targeted
   `input.conf`/`bindings.conf`/`hyprland.conf` with `sed`/append, which fail
   silently (no error) when those files are no longer read.
3. **Package-owned system paths** — the XKB layout was installed to
   `/usr/share/X11/xkb/symbols/`, which an OS update overwrites; only the
   user-level copy under `~/.config/xkb/` survived.
4. **Independent WirePlumber major bump** — 0.5 removed the Lua config API, so a
   separate audio customization broke in the same window and by the same
   class of cause (config-format change).

## Resolution

- Ported live configs to Lua: `input.lua` (`caps:swapescape`, `kb_layout="us,custom"`, `grp:alt_shift_toggle`, `repeat_delay=600`, touchpad `natural_scroll=true`), `bindings.lua` (voxtype SUPER+ALT+V push-to-talk; 200% all-sink volume keys via a native-OSD wrapper), `monitors.lua` (scale 1.25 + `GDK_SCALE=1`), `shell.json` (`"position":"bottom"`).
- Rewrote the audio-suspend workaround in WirePlumber 0.5 SPA-JSON format under `~/.config/wireplumber/wireplumber.conf.d/50-disable-suspend.conf`; retired the dead Lua file.
- Modernized `workstation/omarchy-latest` to write Lua, dropped the redundant manual Nvidia block (Omarchy 4 `nvidia.lua` auto-detects), refined the PipeWire audio config, and switched 200% volume from swayosd (removed in Omarchy 4) to the native OSD.
- Added an Omarchy 4 (Lua) branch to `chinuk-wawa-custom-keyboard/hyprland-install.sh`, guarded against clobbering an existing `custom` layout.

## Lessons Learned

> **"On Omarchy, config edits made by install scripts to `~/.config/hypr/*.conf` are inert under Omarchy 4 — Hyprland reads Lua now. Writes to package-owned system paths (like `/usr/share/X11/xkb/symbols/`) don't survive OS updates; keep user copies under `~/.config`. And WirePlumber 0.5 ignores `main.lua.d/*.lua` — audio rules must be SPA-JSON under `wireplumber.conf.d/`."**

*— Captured 2026-08-17, source: conversation*

## Prevention

- Install scripts should detect the active config format (prefer `input.lua`
  when present, fall back to `input.conf`) and write idempotent, marker-guarded
  blocks — the pattern now used in `omarchy-latest`.
- Keep user-owned copies of anything installed into package-owned system dirs.
- When a major dependency bumps (Hyprland, WirePlumber), verify config-format
  continuity, not just that files still exist on disk.
