# INC-2026-11-17-mics-muted-overgained

**Status:** active
**Tags:** [microphone, internal-mic, dmic, alc257, thinkpad-t490, airpods, hfp, msbc, sco, ax201, wireplumber, pipewire, echo-cancel, rnnoise, default-routes, workstation/omarchy-latest]
**Related:** [DEC-2026-11-17-allow-headset-autoswitch, INC-2026-08-18-audio-fidelity-regression]

## What Happened

On the ThinkPad T490 (`odysseyalive-t490`) only the HP 320 webcam mic worked
reliably. The built-in mic and the AirPods Pro mic "intermittent don't work or
just don't work at all", and the user kept maxing the input volume to get any
signal.

## Timeline

| Time/Commit | Event |
|-------------|-------|
| 2026-11-06 → 11-17 | Kernel logs `Bluetooth: hci0: corrupted SCO packet` / `SCO packet for unknown connection handle 257` on 11-06, 11-10, 11-12, 11-16, 11-17, with msbc already enabled |
| 2026-11-17 | Diagnosed. Internal mic recorded -inf dBFS (muted). Unmuted at volume 1.0 it clipped on fan noise |
| 2026-11-17 | Two attempts at a WebRTC echo-cancel "clean mic" crash-looped PipeWire; removed |
| ef22c8d | Repo fix: T490 mic level block, AirPods HFP made reachable on fresh installs |
| 2026-11-17 | Installing `noise-suppression-for-voice` (extra) for an RNNoise source was denied by the agent permission system; pending |

## Root Cause

Several independent causes, all in session state or config, none in the driver:

1. **Built-in mic muted.** WirePlumber's saved route
   (`~/.local/state/wireplumber/default-routes`,
   `analog-input-internal-mic`) had `"mute":true`, and it was the default
   source. The likely trigger is Omarchy's mic-mute key
   (`omarchy-audio-input-mute` toggles `@DEFAULT_AUDIO_SOURCE@`), but that was
   not proven. The mic-mute LED read 0 while the source was muted, so the mute
   was invisible.
2. **+60 dB at volume 1.0.** On this ALC257 the source volume drives Capture
   (+30 dB max) plus Internal Mic Boost (+30 dB max). At 1.0 the cooling fan
   (3520 RPM; harmonics of 58.7 Hz at 176/294/353/705/1176 Hz) clipped the input:
   RMS -2 dBFS, 43–51% of samples clipped. Effective gain is roughly
   60 + 60·log10(v) dB. "Maxing the volume" made it worse.
3. **AirPods mic route saved at 0.0207 linear (about -34 dB).**
4. **AirPods HFP not auto-connected.** `bluez5.auto-connect` listed only
   `a2dp_sink a2dp_source`. On a fresh install the repo also wrote
   `50-no-autoswitch.conf`, which makes the headset mic unreachable, and a codec
   list without msbc/cvsd (see DEC-2026-11-17-allow-headset-autoswitch).

The internal mic itself is fine: it is a codec DMIC (pin 0x12, `0x90a60120`,
`Conn = Digital`) on `snd_hda_intel`, so SOF is not needed. A 1 kHz tone played
on the speakers appeared 65 dB above its floor. The dmesg line
`ALC257: picked fixup  for PCI SSID 17aa:0000` (real SSID 17aa:22aa) is cosmetic.
The TB3 dock input is a headset jack (`Headset Capture`), not a mic, and reads
zeros with nothing plugged in.

## Contributing Factors (Swiss Cheese Layers)

1. **Persistent state** — WirePlumber restores mute and volume per route, so one
   stray mute or low level outlives restarts and reboots.
2. **Invisible mute** — the mic-mute LED did not reflect the muted source.
3. **No gain cap** — WirePlumber/ACP exposes the full +60 dB range, and
   `api.alsa.soft-mixer = true` on the input node did not detach the route from
   the hardware gain (tested).
4. **Loud fan** — sustained load (~2.4) keeps the fan near 3500 RPM, and its
   noise sits in the speech band.
5. **Config drift** — the live machine had already re-enabled autoswitch and
   msbc, but `workstation/omarchy-latest` still wrote the old files.

## Resolution

- Internal mic unmuted and set to 35% (about +33 dB, no boost). With the fan
  alone, peaks were 10–12 dB under full scale; 40–45% reached full scale in
  3 of 4 takes. Speech level was not measured.
- AirPods route reset to 1.0. `hfp_hf` added to `bluez5.auto-connect`.
- `workstation/omarchy-latest` (ef22c8d): the T490-gated block unmutes the mic
  and sets 35% when it is muted or above 45%. The installer now writes
  `51-airpods-aac.conf` with msbc/cvsd, parks no-autoswitch as `.disabled`, and
  writes autoconnect with `hfp_hf`.
- **Noise suppression is not delivered.** `libpipewire-module-echo-cancel`
  (`aec/libspa-aec-webrtc`, `monitor.mode = true`, stereo and mono) crash-looped
  PipeWire 1.6.9 with `pw.node: can't add port ...: -28, No space left on device`
  followed by SIGSEGV, about six times, and no devices appeared. The config was
  removed. This may be why `62-voxtype-agc.conf` was disabled on 2026-10-04 (it
  uses the same module), but that is unconfirmed.
- **RNNoise pending:** the plan is a filter-chain source using
  `librnnoise_ladspa.so` / `noise_suppressor_mono` on the internal mic, opt-in
  and never the default source. It needs
  `sudo pacman -S --needed noise-suppression-for-voice` (extra repo) first. The
  install was denied in the agent session, so nothing is written or measured yet.

## Lessons Learned

> **"I'm really tired of maxing out the volume just to get a little bit of a mic."**

*— Captured 2026-11-17, source: user*

On this hardware the fix for a quiet mic was less gain plus an unmute. At full
volume the fan noise drowned and clipped the mic.

## Prevention

- Check `wpctl status` for `MUTED` and the saved route in `default-routes` before
  touching gain or drivers.
- Measure (`pw-record` + peak/RMS + spectrum) with the fan RPM logged; a hum at
  RPM/60 harmonics is acoustic fan noise, not a driver fault.
- Do not load `libpipewire-module-echo-cancel` on this machine until the
  ENOSPC/SIGSEGV is understood.
- **Open:** AirPods HFP is untested on hardware since the fix. The AX201 SCO
  corruption is a kernel/firmware issue that config cannot fix and may keep the
  AirPods mic intermittent. RNNoise source not yet installed.
