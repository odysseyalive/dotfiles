# INC-2026-08-20-dock-audio-dac-wedge

**Status:** resolved
**Tags:** [pipewire, wireplumber, thunderbolt, dock, usb-audio, dac, jack-detection, no-fallback, thinkpad-t490, dock-audio-reset, pactl, set-card-profile]
**Related:** [INC-2026-08-18-audio-fidelity-regression]

## What Happened

Audio on the ThinkPad T490 (`odysseyalive-t490`) went completely silent through
the **ThinkPad Thunderbolt 3 Dock** analog output, which was the selected/pinned
default sink. It had been working ~20 minutes earlier with no config change. Every
software indicator was green — sink `Mute: no`, volume 70%, PipeWire stream
`RUNNING`, `hw_ptr` advancing — yet nothing came out the dock's 3.5mm jack. The
symptom read like a mysterious mute even though nothing was muted.

## Timeline

| Time | Event |
|------|-------|
| 2026-08-20, ~10:50 | Dock audio working normally |
| ~11:10 | User reports silence on the dock; no config files changed in the prior 60 min |
| ~11:12 | Diagnosed: software path provably clean end-to-end; isolated to the dock's analog side |
| ~11:13 | `pactl set-card-profile … off` → `… output:analog-stereo+input:mono-fallback` re-opened the ALSA device (node id 59 → 103); DAC revived |
| ~11:15 | One-shot tone via `paplay --device=<dock>` confirmed audible from the dock. Resolved. AirPods (separately just disconnected) reconnected with `bluetoothctl connect`. |

## Root Cause

The dock's USB-audio **DAC wedged** — almost certainly after a Thunderbolt link
renegotiation (a monitor sleeping, a device hotplugged on the dock, or a
power-delivery blip). The USB-audio **card stayed enumerated**, so PipeWire kept
happily streaming to it (`RUNNING`, `hw_ptr` advancing), but the analog output
stage was dead.

Two properties of the device made it look like a software mute and prevented
recovery:

1. **No jack / output-state detection.** Generic USB Audio Class devices like this
   dock (`USB17ef:3083`, `snd_usb_audio`) expose **no jack-presence kcontrol**
   (`amixer -c <n> controls | grep -i jack` → nothing). Every profile port reports
   `available: yes` unconditionally, so nothing can mark the sink unavailable.
2. **No auto-fallback.** Because it was an *explicit* `default.configured.audio.sink`
   pin in `~/.local/state/wireplumber/default-nodes`, WirePlumber would not migrate
   to another sink — and with `session.suspend-timeout-seconds = 0`
   (`50-disable-suspend.conf`) the node never idled out, it just streamed into the
   void.

## Contributing Factors (Swiss Cheese Layers)

1. **Silent hardware fault behind a healthy software stack** — every layer PipeWire
   can see was green, so the fault was invisible to normal diagnostics.
2. **No jack detection** — the OS literally cannot tell the dock's output is dead.
3. **Explicit sink pin + disabled suspend** — removes the two mechanisms
   (auto-fallback, idle-suspend/re-open) that would otherwise self-heal it.
4. **False "software mute" narrative** — the green indicators misdirect toward
   volume/mute hunting when the break is downstream of the USB endpoint.

## Resolution

- **Immediate fix (no sudo):** cycle the dock card profile, which forces PipeWire
  to tear down and re-open the ALSA device (visible as the sink node id changing):

  ```sh
  card=$(pactl list short cards | awk '/Thunderbolt_3_Dock/ {print $2; exit}')
  pactl set-card-profile "$card" off
  sleep 1
  pactl set-card-profile "$card" output:analog-stereo+input:mono-fallback
  ```

- **Packaged as a helper:** `dock-audio-reset` shell function added to the dotfiles
  (`bash/bashrc`, tracked) and to the live `~/.zshrc`, so recovery is one command.
- **AirPods** were a *separate* issue — simply `Connected: no`; reconnected with
  `bluetoothctl connect 74:3F:8E:A8:44:C1` (already Paired/Bonded/Trusted).

## Lessons Learned

> **"A green PipeWire path — unmuted, RUNNING, hw_ptr advancing — proves audio
> reached the device's USB endpoint, not that sound left its analog jack. On a USB
> dock with no jack detection, the software stack cannot see a dead DAC; when every
> indicator is green and it's still silent, suspect the analog side and re-open the
> ALSA device with a card-profile cycle before chasing mutes."**

> **"An explicit sink pin plus disabled node-suspend removes both self-healing paths
> (auto-fallback and idle re-open), so a transient DAC wedge becomes a permanent
> silence until the device is manually re-opened."**

*— Captured 2026-08-20, source: conversation*

## Prevention

- Use `dock-audio-reset` when the dock is selected, everything shows green, and it's
  still silent — before hunting for a nonexistent mute.
- If this recurs frequently, consider dropping the explicit dock sink pin so
  WirePlumber can auto-fallback, or re-enabling idle-suspend for the dock node so it
  re-opens the DAC on its own — both trade against the deliberate choices recorded in
  [[INC-2026-08-18-audio-fidelity-regression]].
