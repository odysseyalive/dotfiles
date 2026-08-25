# DEC-2026-08-24-fidelity-first-flat-dock

**Status:** accepted
**Amends:** [DEC-2026-08-20-dock-lineout-filter-chain-tuning] (reverses its *voicing* choice; keeps its *loudness-parity* and auto-select/re-link choices)
**Tags:** [pipewire, filter-chain, copy-passthrough, channelmix, thunderbolt, dock, usb-audio, line-out, speaker-tuning, thinkpad-t490, fidelity, bitrate, s32, aac, workstation/omarchy-latest]
**Related:** [DEC-2026-08-20-dock-lineout-filter-chain-tuning, INC-2026-08-18-audio-fidelity-regression, INC-2026-08-20-dock-audio-dac-wedge]

## Context

The user asked for a single, consistent way to get **high-fidelity sound across
every output** without hand-tuning each device, and — when given the choice —
picked **native/declarative** management (no third-party session app like
EasyEffects) and an **accurate** signature (correction only, no coloration), plus
an explicit "make sure there is a high bitrate."

This reopens one choice from [[DEC-2026-08-20-dock-lineout-filter-chain-tuning]]:
that record gave the dock line-out a gentle "hi-fi smiley" biquad EQ. A smiley is
a *pleasing coloration*, not fidelity. The dock line-out feeds **external powered
speakers whose response was never measured**, so there is no correct per-device
correction to apply — the faithful action is to pass the signal through untouched
and let the speakers reproduce what is on the recording.

Framing that resolved the user's "why doesn't my tuning carry over" confusion:
audio processing splits into two categories that behave differently across devices.
- **Corrections** (e.g. the T490 Dolby speaker-cabinet convolver) are tied to a
  specific transducer and *cannot* be shared — the same IR on a line-out is the
  wrong transfer function. Per-device by physics, not by config sloppiness.
- **Preferences/voicing** (a smiley) are taste; under an accuracy directive they
  are removed, not shared.
Result: there is no shared "house voicing" layer to factor out — the only genuinely
shared layer is technical hygiene (rates, bit depth, channelmix), which already
lives once in `10-rates.conf`.

## Decision

1. **Dock line-out → accurate/flat.** Replace the smiley biquad EQ in
   `61-dock-speaker-tuning.conf` with a **bit-transparent passthrough** (two
   builtin `copy` nodes, one per channel; no EQ, no gain change). Keep it as a
   filter-chain *sink* — not the raw hardware sink — for two **non-tonal** reasons
   that [[DEC-2026-08-20-dock-lineout-filter-chain-tuning]] established and remain
   valid: it inherits `channelmix.max-volume = 2.0` (loudness parity with the
   internal tuned path), and it preserves the auto-select + `dock-audio-reset`
   re-link-by-name behavior. Old file kept as
   `61-dock-speaker-tuning.conf.smiley.disabled` for A/B.
2. **Internal speakers unchanged.** The T490 Dolby convolver
   (`60-t490-speaker-tuning.conf`) is a genuine driver correction — the accurate
   choice — and stays the default sink.
3. **Bitrate/bit depth verified, not assumed.** Live capture during playback:
   internal DAC negotiates **`S32_LE` @ 48 kHz** (deepest format the hardware
   offers; no 16-bit downgrade), with 44.1/88.2/96/176.4/192 kHz allowed and the
   clock following the source. AirPods already at **AAC VBR `bitratemode = 5`**
   (~256 kbit/s, the AirPods ceiling) via `51-airpods-aac.conf` — the max possible
   over Bluetooth.
4. **A/B helper.** Added `speaker-ab` (`bash/bashrc`, mirrored into live
   `~/.zshrc` since the setup inlines bashrc into zshrc rather than sourcing it):
   flips the internal default between the convolver sink and the raw sink
   instantly (no restart), giving the native/declarative path the on/off ear-check
   that EasyEffects would have provided.
5. **Reproducibility.** Installer block in `workstation/omarchy-latest` updated to
   write the flat dock path (same T490 DMI gate).

## Alternatives Considered

1. **Keep the smiley.** Rejected — it is coloration, contrary to the accuracy
   directive; and the user experienced per-device voicing as inconsistency.
2. **EasyEffects with auto-loaded per-device presets.** Rejected by the user in
   favor of native/declarative; noted as the better tool if the priority ever
   shifts to tweak-by-ear + AutoEQ headphone correction.
3. **Drop the dock filter-chain entirely, use the raw sink.** Rejected — a float32
   passthrough is already transparent, and the raw sink loses the `channelmix`
   loudness headroom and the wedge-recovery re-link. The passthrough sink keeps
   both at zero fidelity cost.
4. **A single always-default "house sound" sink that follows the active device.**
   Not pursued — a static PipeWire filter-chain `target.object` cannot follow the
   default node without a running re-linker daemon (which is essentially what
   EasyEffects is). Under an accuracy directive there is no shared voicing to host
   in such a layer anyway.

## Consequences

- Dock output is now flat/faithful; its honest ceiling is the dock's own modest
  USB DAC, which software cannot lift (hardware limit, stated to the user).
- Internal speakers keep the audible convolver correction; `speaker-ab` makes the
  difference demonstrable on demand.
- No third-party audio app added; whole setup remains declarative and reproducible
  via the installer.
- The voicing guidance in [[DEC-2026-08-20-dock-lineout-filter-chain-tuning]] is
  superseded for the dock; its loudness-parity and DAC-wedge-recovery reasoning
  still stands and is retained here.

## Lessons Learned

> **"Split audio processing into corrections and preferences. A correction is a
> per-transducer transfer function — it cannot 'carry over' to another device, by
> physics. A preference is taste — under an accuracy directive you remove it, you
> don't share it. So 'one setting for hi-fi everywhere' is a category error: the
> only truly shareable layer is technical hygiene (rate/bit-depth/headroom), which
> already lives once in 10-rates.conf. Keep a flat passthrough filter-chain (not
> the raw sink) when you want its channelmix headroom and re-link behavior at zero
> tonal cost."**

*— Captured 2026-08-24, source: conversation*

## Files

- `~/.config/pipewire/pipewire.conf.d/61-dock-speaker-tuning.conf` (now flat passthrough)
- `~/.config/pipewire/pipewire.conf.d/61-dock-speaker-tuning.conf.smiley.disabled` (prior EQ, A/B)
- `~/.config/pipewire/pipewire.conf.d/60-t490-speaker-tuning.conf` (unchanged; convolver)
- `bash/bashrc` + `~/.zshrc` (`speaker-ab` A/B helper)
- `workstation/omarchy-latest` (installer block, DMI-gated)
