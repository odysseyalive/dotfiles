# INC-2026-11-22-airpods-connected-but-silent

**Status:** active
**Tags:** [bluetooth, airpods, hfp, msbc, cvsd, sco, ax201, autoswitch, wireplumber, pipewire, default-source, chrome, webrtc, agc, internal-mic, bt-a2dp-guard, workstation/omarchy-latest]
**Related:** [INC-2026-11-17-mics-muted-overgained, DEC-2026-11-17-allow-headset-autoswitch]

## What Happened

On the ThinkPad T490, the AirPods Pro showed as connected but played and recorded
nothing, often after they came back from the user's phone. The internal mic
volume had also risen to 77% again, above the 45% at which fan noise clips it
(INC-2026-11-17).

## Timeline

| Time/Commit | Event |
|-------------|-------|
| 2026-11-22 | AirPods card found on profile `off` or on a saved `headset-head-unit` after reconnect. Kernel logged `corrupted SCO packet` / `SCO packet for unknown connection handle 257` |
| 2026-11-22 | `bt-a2dp-guard` user service written. It recovered the card from `off` and from an idle headset profile in under 6 s |
| 2026-11-22 | Chrome WebRTC moved the internal mic volume in a test call. `chrome-flags.conf` added |
| 2026-11-22 | Bench tests compared CVSD with mSBC and mid-recording link failure. Guard extended |
| 8df8411 | Guard and Chrome flag mirrored in `workstation/omarchy-latest` |

## Root Cause

1. **Saved headset profile restored on reconnect.** WirePlumber stores the last
   headset profile (`~/.local/state/wireplumber/bluetooth-autoswitch`,
   `saved-headset-profile:...`) and the card profile (`default-profile`). It
   restores them on every reconnect, including the handoff back from the phone.
   voxtype's `~/.local/bin/voxtype-ptt-start` runs
   `pactl set-card-profile ... headset-head-unit` when the default source is
   Bluetooth. It is the likely writer, but that is unproven.
2. **HFP link failure leaves the card on `off`.** When the SCO transport fails,
   the card drops to `off`, which is connected but silent. The `bluez_input`
   loopback source survives that. A recording on it got 15.3 s of digital
   silence. The default source did not move, and the old guard skipped the card
   because the source was RUNNING.
3. **CVSD corrupts the AX201's SCO link; mSBC mostly does not.** These were
   25 s takes from the AirPods mic, counting new kernel `SCO` lines:

   | Codec | Takes | SCO lines per take | Notes |
   |-------|-------|--------------------|-------|
   | CVSD (`headset-head-unit-cvsd`) | 4 | 17, 15, 15, 17 | One take died after 0.6 s |
   | CVSD forced mid-take | 1 | 16 in under 2 s | A 1.6 s gap |
   | mSBC (`headset-head-unit`) | 5 | 0, 0, 0, 1, 0 | Only gaps were the 0.6–1.2 s profile-switch startup, plus one take with four gaps totalling 157 ms |

   One manual pick of CVSD is saved as the headset profile, and autoswitch then
   reuses it. The 11-06 to 11-17 errors may have come from a saved CVSD profile,
   but the earlier saved value is unknown.
4. **The default source follows history order, not priority.** In WirePlumber
   0.5, `state-default-nodes.lua` ranks the default by its place in the stored
   history (`20001 - i`), not by `priority.session`. After any manual pick of the
   internal mic, it beats the AirPods mic away from the desk even when the
   AirPods are connected. This was reproduced with the webcam and dock cards
   switched off.
5. **Chrome moves the OS mic volume.** A headless Chrome 158 WebRTC call
   (getUserMedia into an RTCPeerConnection) changed the internal mic from 35% to
   29% in 12–14 s, in two runs out of two. With
   `--disable-features=WebRtcAllowInputVolumeAdjustment` it held at 35%, in two
   runs out of two. The feature is enabled by default in Chromium
   (`media/webrtc/webrtc_features.cc`). The raise to 77% itself was not observed.

## Contributing Factors (Swiss Cheese Layers)

1. **Persistent state.** WirePlumber restores the card profile, the headset
   profile and the source history, so one bad choice outlives reconnects.
2. **A shared device.** The phone takes the AirPods and hands them back, and
   each return replays the saved state.
3. **A failure that looks like success.** On `off` the source still exists and
   is RUNNING, so apps record silence without any error.
4. **Firmware or kernel.** The AX201 SCO corruption is outside config. USB
   autosuspend is on (`btusb enable_autosuspend = Y`, `1-10/power/control = auto`).
   It could not be tested without root.

## Resolution

`bt-a2dp-guard` (`~/.local/bin`, `~/.config/systemd/user`) watches
`pactl subscribe` card events. After a 3 s grace for voxtype and other recorders
it does the following:
- **Back to stereo.** It puts a Bluetooth card on `off` or a headset profile with
  nothing recording back on `a2dp-sink`.
- **CVSD to mSBC.** It switches `headset-head-unit-cvsd` to `headset-head-unit`.
  In a test the recording continued, and the saved headset profile went back to
  mSBC.
- **Fallback when the link dies while recording.** It sets the default source to
  the highest-`priority.session` ALSA input, moves the source-outputs to it, then
  sets `a2dp-sink`. In a test the recording moved to the internal mic 3–5 s
  after the link died, with a 2.26 s gap, and kept going. At the desk the pick
  would be the webcam (priority 2109).
- **The AirPods mic on connect.** When a card appears and the default source is
  `alsa_input.pci-*`, it makes the `bluez_input` source the default. Tested away
  from the desk: on connect the default was the AirPods mic, and on disconnect
  the internal mic. On a disconnect during a recording, WirePlumber moved the
  stream to the internal mic by itself.

`~/.config/chrome-flags.conf` carries
`--disable-features=WebRtcAllowInputVolumeAdjustment`. The running Chrome needs
a restart to pick it up. The internal mic is back at 35%.

These were tried and did not help:
- **Dropping `cvsd` from `bluez5.codecs`.** The CVSD profile stayed after a
  WirePlumber restart and after a reconnect, because CVSD is mandatory in HFP.
  The change was reverted.
- **`libpipewire-module-echo-cancel`.** Not retried, because of the
  INC-2026-11-17 crash loop.

## Lessons Learned

> **"it isn't professional for me to show up at a job and switch to my airpods and then all of the sudden my mic not work. and it isn't cool to take out my earpods and not have the laptops native mic not work."**

*— Captured 2026-11-22, source: user*

> **"I OFTEN use my phone with my airpods. that process shoulnd't break using my airpods with this system"**

*— Captured 2026-11-22, source: user*

A Bluetooth mic that fails stays visible and RUNNING, so nothing downstream
notices. The fix has to watch the card profile and move the recording itself.
Bench tests change WirePlumber's saved state: a manual CVSD pick and a
`set-default-source` both persist. Restore `bluetooth-autoswitch` and the order
in `default-nodes` after testing.

## Prevention

- **Check saved state first.** When the AirPods are silent, look at
  `bluetooth-autoswitch`, `default-profile` and `default-nodes` before the
  drivers.
- **Test the guard after changes.** After any change to the guard or to the
  autoswitch config, re-run the mid-recording `off` test: the recording must
  land on another mic.
- **Open: a real phone handoff is untested.** It was simulated with
  `bluetoothctl disconnect` and `connect`.
- **Open: speech level on the AirPods mic was not measured.** Takes were
  ambient only.
- **Open: USB autosuspend off is untested** and needs root:
  `echo on | sudo tee /sys/bus/usb/devices/1-10/power/control` now, and
  `options btusb enable_autosuspend=0` in `/etc/modprobe.d/` to keep it.
- **Open: apps that pin a specific mic.** An app that picked a mic by name is
  only covered by the source-output move, which was tested with `pw-record`, not
  with Chrome or Meet.
