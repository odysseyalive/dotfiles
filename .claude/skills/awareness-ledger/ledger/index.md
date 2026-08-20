# Awareness Ledger Index

*Auto-generated. Last updated: 2026-08-20*

## By Tag

- **omarchy / omarchy4 / hyprland** → [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md)
- **workstation/omarchy-latest / install-scripts** → [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md), [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md), [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md)
- **wireplumber / pipewire** → [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md), [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md), [INC-2026-08-20-dock-audio-dac-wedge](INC/INC-2026-08-20-dock-audio-dac-wedge.md), [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md)
- **thunderbolt-dock / usb-audio / dac-wedge / jack-detection** → [INC-2026-08-20-dock-audio-dac-wedge](INC/INC-2026-08-20-dock-audio-dac-wedge.md), [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md)
- **xkb / chinuk-wawa** → [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md)
- **bluetooth / airpods / aac / codec** → [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md)
- **speaker-tuning / filter-chain / convolver / thinkpad-t490** → [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md), [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md)
- **line-out / biquad-eq / channelmix-headroom / clipping-safe** → [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md)

## By Status

### Active

*(No records yet)*

### Resolved

- [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md) — Omarchy 4 migrated Hyprland config `.conf`→Lua (and WirePlumber 0.5 dropped Lua config), silently breaking install-script-applied customizations.
- [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md) — "Tiny/deadpan" audio on all outputs: AirPods stripped of AAC by an SBC-XQ pin, and T490 speakers played flat (no Dolby voicing). Fixed with native AAC@VBR-5 and a PipeWire builtin-convolver speaker tuning (T495 Dolby IRS).
- [INC-2026-08-20-dock-audio-dac-wedge](INC/INC-2026-08-20-dock-audio-dac-wedge.md) — Thunderbolt 3 dock went silent with the whole software path green (unmuted, RUNNING, hw_ptr advancing): the USB-audio DAC wedged after a TB link renegotiation while the card stayed enumerated, and no jack detection + an explicit sink pin blocked recovery. Fixed by cycling the card profile (re-opens the ALSA device); packaged as the `dock-audio-reset` helper.

### Under Review

*(No records yet)*

## Decisions

### Accepted

- [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md) — The dock line-out never inherited the "sound enhancements" because they are device-scoped (A2DP codec knobs = Bluetooth only; the Dolby convolver targets the internal drivers only). Gave the dock its own PipeWire filter-chain sink: a gentle clipping-safe biquad EQ for voicing, plus the inherited `channelmix.max-volume = 2.0` headroom that the raw hardware sink lacked. Baked into `workstation/omarchy-latest`.

## Relationship Map

- [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md) → relates to → [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md) (both: `~/.config` audio customizations lost/at-risk across updates; both fixed in `workstation/omarchy-latest` install).
- [INC-2026-08-20-dock-audio-dac-wedge](INC/INC-2026-08-20-dock-audio-dac-wedge.md) → relates to → [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md) (same T490 audio stack; the explicit dock sink pin and `50-disable-suspend.conf` from the 08-18 fix are the factors that block auto-recovery from the DAC wedge).
- [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md) → resolves gap flagged by → [INC-2026-08-20-dock-audio-dac-wedge](INC/INC-2026-08-20-dock-audio-dac-wedge.md) (the dock had no per-device tuning); builds on the filter-chain pattern from [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md) (EQ instead of a speaker-cabinet convolver, since the dock is a line-out).

## Statistics

| Type | Total | Active | Resolved | Deprecated |
|------|-------|--------|----------|------------|
| Incidents | 3 | 0 | 3 | 0 |
| Decisions | 1 | 1 | 0 | 0 |
| Patterns | 0 | 0 | 0 | 0 |
| Flows | 0 | 0 | 0 | 0 |
