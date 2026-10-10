# INC-2026-10-09-voxtype-egpu-and-hotplug

**Status:** active (Voxtype on eGPU verified; hot-plug with `pci=nocrs` awaits its first reboot-then-power-on test)
**Tags:** voxtype, whisper, vulkan, egpu, node-titan, quadro-rtx-5000, thunderbolt, pci-bar, limine, kernel-cmdline, thinkpad-t490, workstation/omarchy-latest, voxtype.service.d/egpu.conf, /etc/limine-entry-tool.d/egpu-hotplug.conf
**Related:** INC-2026-08-20-dock-audio-dac-wedge (same T490 Thunderbolt port)

## What Happened

The user asked whether Voxtype dictation could use the AKiTiO Node Titan eGPU (Quadro RTX 5000). Two separate faults surfaced:

1. **Hot-plug never worked.** The Node Titan powered on after boot enumerated, but the nvidia driver refused it: `NVRM: BAR1 is 0M @ 0x0`, `probe with driver nvidia failed with error -1`. The earlier fix (a162f56, `pci=realloc,hpmmiosize=32M,hpmmioprefsize=1G`) was active and still failed: at boot the kernel logged `00:1c.4: bridge window [mem size 0x40000000 64bit pref]: can't assign; no space`.
2. **Voxtype ignored the eGPU even when it worked.** With the card up (Titan on at boot), Voxtype's Vulkan build still transcribed on the Intel iGPU: 34s of audio took 15s.

## Timeline

| Time/Commit | Event |
|-------------|-------|
| a162f56 (2026-10-07) | Added hpmmio* reservation drop-in; one reboot applied it |
| 2026-10-09 16:35 | Titan hot-plugged; `BAR1 is 0M`, driver probe fails |
| 2026-10-09 | Found ACPI host bridge window is 32-bit only; enabled `whisper.gpu_isolation` |
| 5c3e979 | Added `pci=nocrs`; drop-in now rewritten when its contents differ |
| 2026-10-09 16:52 | User rebooted with Titan on; card works, but `/proc/cmdline` had no `pci=` params and a stray `"` (pasted command wrapped mid-string) |
| 2026-10-09 | Voxtype confirmed on iGPU; DRI_PRIME drop-in added; live test on Quadro |
| a53a63c | Voxtype eGPU drop-in baked into `workstation/omarchy-latest` |
| 2026-10-09 | User replaced the broken limine drop-in by copying a prepared file |

## Root Cause

1. **Hot-plug:** the T490's ACPI `_CRS` gives the kernel only one memory window, `0x8f800000-0xdfffffff` (32-bit), which already holds the iGPU's 512M aperture at `0xa0000000`. No 1G-aligned 1G block fits, so the reservation failed at boot, and on hot-plug the Quadro's 256M prefetchable BAR1 had nowhere to go.
2. **Voxtype:** whisper.cpp/ggml takes the first Vulkan device, and `ggml_vulkan` lists the Intel iGPU as device 0 and the Quadro as device 1.

## Contributing Factors (Swiss Cheese Layers)

1. **Firmware** — `_CRS` lists no window above 4G, although the CPU has 39-bit physical addressing.
2. **Kernel param sizing** — `hpmmioprefsize` also applies to the Titan's nested hot-plug bridges (07:04.0), inflating the request (the hot-add asked for 2G).
3. **Device order** — ggml lists every Vulkan device and does not prefer the discrete one.
4. **Environment inheritance** — `voxtype-osd-gtk4` is a child of the daemon, so any GPU-selection env var also moved the OSD (a GTK4 window) onto the eGPU, where it held 6-7 MiB permanently.
5. **Operator** — a long `sudo printf … | tee` command pasted into a terminal wrapped inside the quoted string and wrote a broken drop-in that passed silently.

## Resolution

- `pci=nocrs` added: kernel cmdline is `pci=realloc,nocrs,hpmmiosize=32M,hpmmioprefsize=1G` via `/etc/limine-entry-tool.d/egpu-hotplug.conf`. If a boot hangs, press E in Limine and delete `nocrs,`.
- `~/.config/systemd/user/voxtype.service.d/egpu.conf`: `Environment=DRI_PRIME=1 GSK_RENDERER=cairo GDK_DISABLE=gl,vulkan`.
- `whisper.gpu_isolation = true`: each transcription runs in a child that exits, so device selection happens per dictation and the eGPU is free between dictations.
- Verified: live dictation ran on the Quadro (469 MiB), 4s of audio transcribed in 0.5s, card released after; OSD not on the Quadro. With NVIDIA hidden (`VK_LOADER_DRIVERS_DISABLE='*nvidia*'`), selection falls back to the iGPU.

## Lessons Learned

> **"whisper.gpu_device=1 looks like the obvious fix, but when the eGPU is off an out-of-range index drops to CPU (52s for 5s of audio). Reorder devices with DRI_PRIME=1 instead, which degrades to the iGPU."**

> **"Any GPU env var on voxtype.service also lands on the OSD child. Check `nvidia-smi` for voxtype-osd-gtk4 after changing it."**

> **"Check `/proc/cmdline` after a reboot. The limine drop-in can be malformed and the boot still succeeds."**

*— Captured 2026-10-09, source: conversation*

## Prevention

- Hand the user a prepared file to `sudo cp` rather than a long one-liner to paste.
- After changing kernel params, confirm with `grep -o 'pci=[^ ]*' /proc/cmdline`.
- Confirm Voxtype's device with a non-isolated run: `voxtype -c <copy with gpu_isolation=false> transcribe x.wav 2>&1 | grep -E 'ggml_vulkan|using Vulkan'` (the isolated worker does not log ggml lines).
- Close this record once a reboot with the Titan off, followed by power-on, shows the Quadro in `nvidia-smi`.
