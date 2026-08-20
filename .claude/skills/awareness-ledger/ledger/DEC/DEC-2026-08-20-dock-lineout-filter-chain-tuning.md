# DEC-2026-08-20-dock-lineout-filter-chain-tuning

**Status:** accepted
**Tags:** [pipewire, wireplumber, filter-chain, biquad, channelmix, thunderbolt, dock, usb-audio, line-out, speaker-tuning, thinkpad-t490, clipping-safe, workstation/omarchy-latest]
**Related:** [INC-2026-08-20-dock-audio-dac-wedge, INC-2026-08-18-audio-fidelity-regression]

## Context

After the 08-18 fidelity work, the ThinkPad Thunderbolt 3 Dock line-out still
sounded **quiet and flat** relative to the internal speakers and the AirPods. The
user read this as the "recent sound enhancements" no longer extending to the dock,
and suspected the newly-added `dock-audio-reset` helper (INC-2026-08-20) had
regressed it.

Review showed no regression. The two fidelity enhancements are **device-scoped by
construction** and never covered the dock:

1. **AAC codec tuning** (`51-airpods-aac.conf`, `50-no-autoswitch.conf`) operates on
   the **Bluetooth A2DP** codec negotiation. The dock is USB Audio Class — no A2DP,
   so the knobs are physically inapplicable.
2. **Dolby speaker convolver** (`60-t490-speaker-tuning.conf`) hard-targets
   `alsa_output.pci-0000_00_1f.3.analog-stereo` — the **internal PCI speakers only**.
   The dock USB sink is never routed through it.

Two structural causes of "quiet + flat" on the dock:

- **No loudness headroom.** A filter-chain sink inherits `channelmix.max-volume =
  2.0` from `10-rates.conf` (stream.properties); the internal tuned sink rides that
  200% ceiling. The **raw dock hardware sink is capped at its 0 dB base volume**, so
  on the same volume keybind the dock is quieter.
- **No voicing.** Plain USB Audio Class output with no per-device correction plays
  flat ("deadpan").

`dock-audio-reset` only cycles the card profile; it touches no volume or routing.

## Decision

Route the dock through a **native PipeWire filter-chain sink**
(`effect_input.dock-tuned-output` → targets the raw dock sink), mirroring the
internal-speaker architecture, with:

- **Loudness:** the filter-chain sink inherits `channelmix.max-volume = 2.0`,
  closing the headroom gap structurally (same mechanism the internal tuned sink
  uses). Raw dock sink demoted to `priority.session = 100`
  (`71-dock-raw-speaker-priority.conf`); tuned sink at `2000` so the dock
  auto-selects tuned. WirePlumber pin updated to the tuned sink.
- **Voicing:** a gentle hi-fi "smiley" via **builtin biquads** — `bq_lowshelf`
  +1.5 dB @ 110 Hz (warmth) and `bq_highshelf` +1.5 dB @ 9 kHz (air), tuned for the
  powered desktop speakers on the dock jack.
- **Clipping-safe by construction:** a final `linear` stage (`mult = 0.841`,
  −1.5 dB) offsets the shelf boost so no band exceeds unity — honoring the same
  unity-peak discipline as the internal IR. Perceived loudness is made up with the
  volume control / the speakers' own amp, not positive gain in the chain.

Baked into `workstation/omarchy-latest` (DMI-gated to the T490, like the internal
block) so a reinstall reproduces it, per the 08-18 prevention note.

## Alternatives Considered

1. **Reuse the T490 Dolby speaker-cabinet IR on the dock.** Rejected — that IR
   corrects the T490's *internal drivers*; applied to a line-out feeding external
   powered speakers it is the wrong transfer function.
2. **Just raise the dock volume / rely on soft-mixer >100%.** Rejected as the sole
   fix — leaves the sound flat and does not give the dock the channelmix headroom
   the tuned path has; a filter-chain provides both.
3. **Drop the explicit dock sink pin so WirePlumber auto-falls-back** (the other
   INC-2026-08-20 prevention option). Orthogonal — addresses DAC-wedge recovery,
   not fidelity. Not taken here; the tuned sink keeps a pin.

## Consequences

- The dock now has loudness parity with the internal tuned path and a gentle
  voicing; verified live (sink RUNNING/default, `channelmix.max-volume = 2.0`,
  `pw-link` shows `effect_output.dock-tuned-output → dock analog-stereo`, test tone
  audible end-to-end).
- **`dock-audio-reset` still works:** after a card-profile cycle the filter-chain
  re-links to the recreated dock sink automatically (target matched by name).
- Voicing is EQ, not measured correction — tweak `Gain` values to taste; if shelves
  are raised, lower the `linear` `mult` (`10^(−maxGainDB/20)`) to stay clipping-safe.
- A/B or disable: rename `61-dock-speaker-tuning.conf` to `.conf.disabled` and
  restart pipewire; the pin falls back to the raw dock sink.

## Lessons Learned

> **"'Sound enhancements' are device-scoped: A2DP codec knobs touch only Bluetooth,
> and a speaker-cabinet convolver touches only the drivers it targets. A USB dock
> line-out inherits neither — give it its own filter-chain. The single mechanism
> that makes a filter-chain sink louder than a raw hardware sink is the inherited
> `channelmix.max-volume` headroom; the raw sink is capped at its 0 dB base."**

*— Captured 2026-08-20, source: conversation*

## Files

- `~/.config/pipewire/pipewire.conf.d/61-dock-speaker-tuning.conf`
- `~/.config/wireplumber/wireplumber.conf.d/71-dock-raw-speaker-priority.conf`
- `~/.local/state/wireplumber/default-nodes` (pin → `effect_input.dock-tuned-output`)
- `workstation/omarchy-latest` (installer block, DMI-gated)
