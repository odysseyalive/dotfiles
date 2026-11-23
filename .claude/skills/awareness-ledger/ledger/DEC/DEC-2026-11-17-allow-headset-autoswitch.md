# DEC-2026-11-17-allow-headset-autoswitch

**Status:** accepted
**Tags:** [bluetooth, airpods, hfp, autoswitch, msbc, wireplumber, 50-no-autoswitch.conf, 51-airpods-aac.conf, bluetooth-a2dp-autoconnect.conf, workstation/omarchy-latest]
**Related:** [INC-2026-11-17-mics-muted-overgained, INC-2026-08-18-audio-fidelity-regression, INC-2026-11-22-airpods-connected-but-silent]

## Context

INC-2026-08-18 added `50-no-autoswitch.conf`
(`bluetooth.autoswitch-to-headset-profile = false`) so AirPods stayed on stereo
AAC when an app opened the mic. That made the AirPods mic unreachable. The live
machine had already renamed the file to `.disabled` and added msbc/cvsd to
`bluez5.codecs` in `51-airpods-aac.conf`. `workstation/omarchy-latest` still wrote
the old files (`50-no-autoswitch.conf` and `51-bluetooth-aac.conf` with
`[ aac sbc_xq sbc ]`), so a fresh install would remove the AirPods mic again.

## Decision Drivers

- The user wants "all available mics to work, even my airpods if connected".
- Repo and live config must match, and only one file should set `bluez5.codecs`.

## Options Considered

### Option A: Keep no-autoswitch (status quo in repo)

- Good, because playback stays stereo AAC during calls.
- Bad, because the AirPods mic never becomes available.

### Option B: Allow autoswitch, make HFP reachable

- Good, because the AirPods mic works while an app records.
- Bad, because playback drops to mono HFP (mSBC/CVSD) for that time and returns
  to AAC afterwards.

## Decision

Chosen option: **Option B**. This reverses the no-autoswitch part of
INC-2026-08-18; its AAC VBR-5 part stands. In ef22c8d the installer:
- moves any existing `50-no-autoswitch.conf` to `.disabled`;
- writes `51-airpods-aac.conf` with `[ aac sbc_xq sbc msbc cvsd ]` and removes
  `51-bluetooth-aac.conf`;
- writes `bluetooth-a2dp-autoconnect.conf` with
  `bluez5.auto-connect = [ a2dp_sink a2dp_source hfp_hf ]`.

The installed `man 7 pipewire-props` describes `bluez5.codecs` as A2DP codecs.
PipeWire 1.6.9 also ships the HFP codecs as plugins
(`libspa-codec-bluez5-hfp-msbc.so` and `-hfp-cvsd.so`), so msbc/cvsd are kept in
the list as the safe choice.

> **"I want all available mics to work, even my airpods if connected."**

*— Captured 2026-11-17, source: user*

## Consequences

- AirPods playback is mono while any app records from them.
- To trade the mic back for stereo during calls, rename
  `50-no-autoswitch.conf.disabled` back.
- HFP quality on this machine is still exposed to the Intel AX201
  `corrupted SCO packet` issue (INC-2026-11-17-mics-muted-overgained).
- **2026-11-22 correction (INC-2026-11-22-airpods-connected-but-silent).**
  "msbc/cvsd kept" is only half safe. CVSD logged 15–17 `corrupted SCO packet`
  per 25 s take, against 1 in 5 takes for mSBC. One manual CVSD pick is saved
  and then reused by autoswitch.
  - Removing `cvsd` from `bluez5.codecs` does not remove the CVSD profile (tested;
    CVSD is mandatory in HFP), so the list stays as it is.
  - The `bt-a2dp-guard` user service switches CVSD to mSBC.
  - The guard also returns the card to `a2dp-sink` when the mic is idle or the
    card is stuck on `off`. That covers the saved headset profile restored when
    the AirPods come back from the phone.
  - When the HFP link dies mid-recording, the guard moves the default source and
    the streams to the best ALSA mic.
  - On connect it makes the AirPods mic the default if the internal mic was.
  - The user kept the AirPods mic with this guard rather than going back to
    Option A.

## Confirmation Criteria

Right if the AirPods, connected and selected as the input, record speech at a
usable level without raising volume. Reconsider if the AX201 SCO corruption makes
HFP unusable; then the built-in/webcam mic plus stereo A2DP (Option A) may be the
better default. Not yet confirmed on hardware (AirPods out of range during the
fix).
