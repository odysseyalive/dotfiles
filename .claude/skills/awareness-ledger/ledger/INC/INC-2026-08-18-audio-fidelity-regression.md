# INC-2026-08-18-audio-fidelity-regression

**Status:** resolved
**Tags:** [pipewire, wireplumber, bluetooth, airpods, aac, sbc-xq, speaker-tuning, filter-chain, convolver, thinkpad-t490, ~/.config/pipewire, ~/.config/wireplumber, workstation/omarchy-latest]
**Related:** [INC-2026-08-17-omarchy4-conf-to-lua-migration]

## What Happened

After recent system updates, audio on the ThinkPad T490 (`odysseyalive-t490`) felt
"tiny and deadpan" across **every** output — built-in speakers, AirPods Pro, and
dock. The user recalled previously "high definition" sound and believed a recent
Omarchy/package update had regressed it, and that adjusting the output **sample
rate** would restore it.

## Timeline

| Time/Commit | Event |
|-------------|-------|
| 2026-07-11 → 07-26 | PipeWire 1.6.4→1.6.8, WirePlumber 0.5.14→0.5.15, bluez 5.86→5.87 (minor point releases) |
| 2026-08-17 | User added `bluetooth-codec-sbc-xq.conf` (pinned `bluez5.codecs = [ sbc_xq ]`, stripping AAC) and `50-disable-suspend.conf` as a fix attempt; sound still thin |
| 2026-08-18 | Diagnosed and resolved (this record) |

## Root Cause

Two independent problems, neither caused by the package updates:

1. **AirPods:** the SBC-XQ pin removed AAC (the AirPods' native codec) entirely.
   Even without the pin, PipeWire defaults AAC to CBR ~128 kbit/s ("AAC-LQ"),
   the classic muffled mode.
2. **Speakers:** Linux plays the T490 speakers **flat**. Windows applies a Dolby
   convolution that voices them; Linux does not. Omarchy's `omarchy audio tuning`
   ships only a `dell-xps-2026` profile keyed on Dell DMI SKU — `omarchy audio
   tuning match` returns nothing on the T490 — so this machine never had any
   speaker voicing. The remembered "high definition" was not from Omarchy tuning.

Sample rate was a red herring: `10-rates.conf` already allowed up to 192 kHz, and
"tiny/deadpan" is a tonal-balance (frequency-response) problem, not a temporal
(sample-rate) one.

## Contributing Factors (Swiss Cheese Layers)

1. **Point-release coincidence** — updates landed near the complaint, creating a
   false "regression" narrative that misdirected the search.
2. **Accumulated user tweaks** — `api.alsa.soft-mixer=true` (digital volume,
   bypasses hardware amp), `channelmix.max-volume=2.0` (a *lowered* volume
   ceiling, not a boost), `resample.quality=14` (no audible gain, ~3× CPU), and
   the SBC-XQ pin each shaved quality or added confusion.
3. **Hardware-gated tuning** — Omarchy's speaker tuning matches Dell SKUs only;
   the T490 is silently unsupported, with no fallback.
4. **Wrong mental model** — sample rate assumed to be the fidelity lever.

## Resolution

- **AirPods** — `~/.config/wireplumber/wireplumber.conf.d/51-airpods-aac.conf`
  (`bluez5.codecs = [ aac sbc_xq sbc ]`, `bluez5.a2dp.aac.bitratemode = 5`) and
  `50-no-autoswitch.conf` (`bluetooth.autoswitch-to-headset-profile = false`, so
  a mic-grab no longer drops them to mono HFP). SBC-XQ pin kept as
  `bluetooth-codec-sbc-xq.conf.disabled` for A/B. Confirmed live:
  `api.bluez5.codec = "aac"`.
- **Speakers** — native PipeWire filter-chain
  `~/.config/pipewire/pipewire.conf.d/60-t490-speaker-tuning.conf` using the
  **builtin convolver** (no third-party program) with the **ThinkPad T495**
  `DolbyMusicBalanced` impulse response — the T495 is the identical chassis and
  speaker hardware to the T490 (only the CPU differs). IR copied to
  `~/.config/pipewire/irs/t495-dolby-music.wav` and **normalized to unity peak
  magnitude (−14.8 dB)** so no band clips. Auto-selected via
  `70-t490-raw-speaker-priority.conf` (tuned sink `priority.session = 1010`, raw
  analog demoted to `100`). IR source:
  `github.com/shuhaowu/linux-thinkpad-speaker-improvements`.
- User confirmed speakers now sound "much much better."

## Lessons Learned

> **"Sample rate can't add body the speakers never reproduced; a convolution
> correction can. 'Tiny and deadpan' is a tonal-balance problem, not a
> sample-rate one."**

> **"The T490 and T495 share identical speaker hardware, so the T495 Dolby IRS is
> a drop-in correction — and PipeWire's own builtin convolver runs it natively,
> no EasyEffects/third-party app needed. It is the same engine EasyEffects wraps
> and the same filter-chain mechanism Omarchy's speaker tuning uses."**

*— Captured 2026-08-18, source: conversation*

## Prevention

- Bake the audio config into the `workstation/omarchy-latest` install so it is
  reproducible on reinstall — the fix otherwise lives only in `~/.config` and
  would be lost like the customizations in
  [[INC-2026-08-17-omarchy4-conf-to-lua-migration]].
- A convolver IR is normalized at author time; if a different profile is adopted,
  re-run the unity-peak normalization to keep it clipping-safe.
