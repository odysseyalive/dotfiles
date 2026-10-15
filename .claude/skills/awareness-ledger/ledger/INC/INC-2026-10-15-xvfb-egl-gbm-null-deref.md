# INC-2026-10-15-xvfb-egl-gbm-null-deref

**Status:** active (fix committed and tested in a sandbox; `/usr/local/bin/Xvfb` is installed on the next sudo run of `workstation/omarchy-latest`)
**Tags:** xvfb, egl, libglvnd, egl-gbm, nvidia, mesa, swrast_dri, bwrap, sandbox, __EGL_VENDOR_LIBRARY_FILENAMES, workstation/omarchy-latest, workstation/scripts/Xvfb, /usr/local/bin/Xvfb
**Related:** INC-2026-10-09-voxtype-egpu-and-hotplug (same NVIDIA eGPU stack on this machine)

## What Happened

On 2026-10-15 Xvfb aborted with SIGABRT several times. A crash-diagnosis session in `~/Work` traced the aborts to a NULL dereference inside NVIDIA's egl-gbm, which runs whenever Xvfb is started where `/dev/dri` is not visible, such as in a sandbox.

## Timeline

| Time/Commit | Event |
|-------------|-------|
| 2026-10-15 | Xvfb SIGABRT crashes; diagnosed in the `~/Work` crash-diagnosis session |
| 2026-10-15 | Reproduced: `bwrap --dev-bind / / --dev /dev --unshare-pid Xvfb :97` segfaults at address 0x0 |
| 2026-10-15 | Forcing Mesa's EGL vendor stops the crash under the same bwrap setup |
| 2026-10-15 | `workstation/scripts/Xvfb` wrapper and its `omarchy-latest` install step added; the wrapper ran 6s under bwrap without aborting (stopped by `timeout`) |

## Root Cause

Xvfb's GLX loads `swrast_dri`, which asks libglvnd for a default GBM EGL display. `10_nvidia.json` sorts before `50_mesa.json` in `/usr/share/glvnd/egl_vendor.d/`, so the request goes to NVIDIA's egl-gbm 1.1.4. Its `OpenDefaultDrmDevice()` (`gbm-display.c:139`) checks only `drmGetDevices2() == 0`. When the call returns a negative error (no `/dev/dri`), it dereferences NULL. NVIDIA/egl-gbm main still has the bug.

## Contributing Factors (Swiss Cheese Layers)

1. **Upstream bug**: egl-gbm treats only a zero device count as failure and misses negative error returns.
2. **Vendor order**: libglvnd tries vendors in filename order, so NVIDIA is asked first even by a software-only X server.
3. **Environment**: sandboxes (bwrap with `--dev /dev`) hide `/dev/dri`, which triggers the error path.
4. **PATH order**: `~/.local/bin` comes after `/usr/bin` here, so a user-level wrapper would be shadowed. It has to go in `/usr/local/bin`, which needs sudo.

## Resolution

`workstation/scripts/Xvfb` sets `__EGL_VENDOR_LIBRARY_FILENAMES` to `50_mesa.json` unless the caller already set it, then execs `/usr/bin/Xvfb`. `workstation/omarchy-latest` installs it with `sudo install -Dm755` to `/usr/local/bin/Xvfb`, from the checkout being run (falling back to `~/.yadrlite`), and only when the installed copy differs.

## Lessons Learned

> **"Do not run Xvfb or anything else GL-related against the host /dev."** Test only inside bwrap with `--dev /dev` and `--die-with-parent`, and remove any `/tmp/.X<N>-lock` it leaves behind.

*— Captured 2026-10-15, source: user, relayed by the `~/Work` crash-diagnosis session*

## Prevention

Drop the wrapper once an egl-gbm release checks `drmGetDevices2()` for negative returns. Until then, re-running `omarchy-latest` keeps `/usr/local/bin/Xvfb` current.
