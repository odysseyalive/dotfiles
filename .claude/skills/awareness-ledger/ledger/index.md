# Awareness Ledger Index

*Auto-generated. Last updated: 2026-09-01*

## By Tag

- **workstation/nvim / lazyvim / codecompanion / openrouter / copilot / theme.lua** → [DEC-2026-09-01-nvim-openrouter-ai-and-theme-reconcile](DEC/DEC-2026-09-01-nvim-openrouter-ai-and-theme-reconcile.md)

- **omarchy / omarchy4 / hyprland** → [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md)
- **workstation/omarchy-latest / install-scripts** → [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md), [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md), [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md), [DEC-2026-08-24-fidelity-first-flat-dock](DEC/DEC-2026-08-24-fidelity-first-flat-dock.md)
- **wireplumber / pipewire** → [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md), [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md), [INC-2026-08-20-dock-audio-dac-wedge](INC/INC-2026-08-20-dock-audio-dac-wedge.md), [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md), [DEC-2026-08-24-fidelity-first-flat-dock](DEC/DEC-2026-08-24-fidelity-first-flat-dock.md)
- **thunderbolt-dock / usb-audio / dac-wedge / jack-detection** → [INC-2026-08-20-dock-audio-dac-wedge](INC/INC-2026-08-20-dock-audio-dac-wedge.md), [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md), [DEC-2026-08-24-fidelity-first-flat-dock](DEC/DEC-2026-08-24-fidelity-first-flat-dock.md)
- **fidelity / bitrate / bit-depth (s32) / accurate-vs-coloration** → [DEC-2026-08-24-fidelity-first-flat-dock](DEC/DEC-2026-08-24-fidelity-first-flat-dock.md)
- **xkb / chinuk-wawa** → [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md)
- **bluetooth / airpods / aac / codec** → [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md)
- **speaker-tuning / filter-chain / convolver / thinkpad-t490** → [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md), [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md), [DEC-2026-08-24-fidelity-first-flat-dock](DEC/DEC-2026-08-24-fidelity-first-flat-dock.md)
- **line-out / biquad-eq / channelmix-headroom / clipping-safe** → [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md), [DEC-2026-08-24-fidelity-first-flat-dock](DEC/DEC-2026-08-24-fidelity-first-flat-dock.md)

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

- [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md) — The dock line-out never inherited the "sound enhancements" because they are device-scoped (A2DP codec knobs = Bluetooth only; the Dolby convolver targets the internal drivers only). Gave the dock its own PipeWire filter-chain sink: a gentle clipping-safe biquad EQ for voicing, plus the inherited `channelmix.max-volume = 2.0` headroom that the raw hardware sink lacked. Baked into `workstation/omarchy-latest`. **(Voicing since superseded by [DEC-2026-08-24-fidelity-first-flat-dock](DEC/DEC-2026-08-24-fidelity-first-flat-dock.md); loudness-parity + wedge-recovery reasoning retained.)**
- [DEC-2026-08-24-fidelity-first-flat-dock](DEC/DEC-2026-08-24-fidelity-first-flat-dock.md) — Fidelity-first directive (native, accurate, high bitrate). Split audio processing into *corrections* (per-transducer, unshareable — e.g. the T490 convolver, kept) vs *preferences* (taste — removed under accuracy). Dock line-out changed from a "smiley" EQ to a bit-transparent `copy` passthrough (keeps `channelmix` loudness headroom + wedge-recovery re-link at zero tonal cost). Verified DAC at `S32_LE`/48 kHz and AirPods at AAC VBR-5. Added `speaker-ab` A/B helper. EasyEffects considered and declined.

- [DEC-2026-09-01-nvim-openrouter-ai-and-theme-reconcile](DEC/DEC-2026-09-01-nvim-openrouter-ai-and-theme-reconcile.md) — Hybrid nvim AI stack: keep Copilot for inline completion (flat-rate, low-latency FIM), add CodeCompanion via the built-in OpenRouter adapter for chat/agentic (frontier model choice), retire CopilotChat. Reconciled `workstation/nvim` with the live Omarchy machine and renamed `kitty-themes.lua` → `theme.lua` (Omarchy-symlink compatible; keeps SeaShells as the off-Omarchy fallback).

## Relationship Map

- [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md) → relates to → [INC-2026-08-17-omarchy4-conf-to-lua-migration](INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md) (both: `~/.config` audio customizations lost/at-risk across updates; both fixed in `workstation/omarchy-latest` install).
- [INC-2026-08-20-dock-audio-dac-wedge](INC/INC-2026-08-20-dock-audio-dac-wedge.md) → relates to → [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md) (same T490 audio stack; the explicit dock sink pin and `50-disable-suspend.conf` from the 08-18 fix are the factors that block auto-recovery from the DAC wedge).
- [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md) → resolves gap flagged by → [INC-2026-08-20-dock-audio-dac-wedge](INC/INC-2026-08-20-dock-audio-dac-wedge.md) (the dock had no per-device tuning); builds on the filter-chain pattern from [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md) (EQ instead of a speaker-cabinet convolver, since the dock is a line-out).
- [DEC-2026-08-24-fidelity-first-flat-dock](DEC/DEC-2026-08-24-fidelity-first-flat-dock.md) → amends → [DEC-2026-08-20-dock-lineout-filter-chain-tuning](DEC/DEC-2026-08-20-dock-lineout-filter-chain-tuning.md) (reverses the dock *voicing* to a flat passthrough under a fidelity-first/accuracy directive; keeps its loudness-parity and DAC-wedge re-link reasoning). Preserves the convolver correction from [INC-2026-08-18-audio-fidelity-regression](INC/INC-2026-08-18-audio-fidelity-regression.md) as the accurate internal-speaker choice.

## Statistics

| Type | Total | Active | Resolved | Deprecated |
|------|-------|--------|----------|------------|
| Incidents | 3 | 0 | 3 | 0 |
| Decisions | 3 | 3 | 0 | 0 |
| Patterns | 0 | 0 | 0 | 0 |
| Flows | 0 | 0 | 0 | 0 |
