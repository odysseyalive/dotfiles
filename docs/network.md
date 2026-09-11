# Network

Tools for the failure mode where the connection is "up", reports 0% packet
loss, and still stutters: streaming audio clips, and interactive SSH inside
tmux freezes and then catches up in a burst.

[Return to top](#yadrlite)

## Why "0% packet loss" hides the problem

Wi-Fi retransmits a failed frame at the radio layer rather than dropping it.
TCP therefore never sees loss — it sees a *latency spike*. Your session does
not die, it hangs for a moment and resumes. Every diagnostic that reports
averages (`ping -c 4`, speed-test sites) calls this healthy, because on
average it is.

The numbers that actually expose it:

| Signal | Healthy | Meaning when bad |
|---|---|---|
| Wi-Fi **tx retry rate** | under 5% | Airtime is gone; frames are being re-sent |
| Latency **jitter** (mdev) | under 10ms | Buffers under-run; audio clips |
| **max vs min** RTT | within ~8x | Traffic parked behind retries |
| **APs on your channel** | 1–2 | Neighbours consuming your airtime |

## `net-doctor`

Read-only. Reports band, signal, retry rate, co-channel neighbours, Bluetooth
coexistence, latency jitter, and DNS timing, then ranks what to fix.

```bash
net-doctor                        # full report
NET_DOCTOR_LOAD=1 net-doctor      # generate traffic so retry rate is meaningful
NET_DOCTOR_SAMPLE=30 net-doctor   # longer retry sample
NET_DOCTOR_PINGS=100 net-doctor   # more latency probes
```

**Retry rate needs traffic to mean anything.** On a near-idle link the few
frames sent are mostly low-rate management and probe traffic, which retries
naturally high — the same link here measured **19.5% over 195 frames** and
**5.4% over 1,935 frames** minutes apart. Below 500 frames `net-doctor` prints
the number but withholds a verdict. Use `NET_DOCTOR_LOAD=1`, or run it while
streaming or copying a file.

## `wifi-band-prefer`

Most routers publish 2.4 GHz and 5 GHz under one SSID. Clients often stay on
2.4 GHz because it is louder — but louder is not faster, and 2.4 GHz is where
every neighbour, microwave, printer SoftAP, and Bluetooth device also lives.

```bash
wifi-band-prefer status   # which band you are on, and what else the SSID offers
wifi-band-prefer 5        # prefer 5 GHz, keeping 2.4 GHz as fallback
wifi-band-prefer auto     # undo
```

It creates a second NetworkManager profile (`"<SSID> (5GHz)"`, `band=a`,
higher autoconnect priority) rather than locking your existing profile, so if
the 5 GHz radio is out of range NetworkManager falls back to 2.4 GHz instead
of leaving you offline.

5 GHz does not travel as far. If `net-doctor` shows the 5 GHz signal weaker
than about **-75 dBm** at your desk, fix the AP's channel instead of forcing
the band.

### Bluetooth and Wi-Fi share one radio

On Intel CNVi cards (AX200/AX201/AX211, common in ThinkPads) Wi-Fi and
Bluetooth sit behind a **single shared RF front-end**. With Wi-Fi on 2.4 GHz,
streaming Bluetooth audio forces the two to time-share, and both degrade.

Do **not** "fix" this with `iwlwifi.bt_coex_active=0`, a popular suggestion
online. Coexistence is what makes the two schedule *around* each other;
disabling it makes them transmit *over* each other. Move Wi-Fi to 5 GHz, or
use a wired output, instead.

## SSH over a link that jitters

`ssh/resilience.conf` supplies generic `Host *` fallbacks, included from the
end of `~/.ssh/config` by the installer:

- **`ServerAliveInterval 20` / `CountMax 3`** — reclaim a dead session in ~60s
  instead of OpenSSH's 3 minutes of silence.
- **`TCPKeepAlive yes`** — catches the case where the route dies, not the daemon.
- **`ControlMaster auto` / `ControlPersist 10m`** — reuse one connection for
  every later `ssh`/`scp`/`rsync` to the same host. Measured here: **0.85s cold
  vs 0.18s reused.** Hosts reached through a `ProxyCommand` bastion pay the
  handshake twice, so they gain the most.

Two things to know:

1. The Include **must** be preceded by a bare `Host *` line. An `Include`
   inherits whatever block it sits in, so appended after your last
   `Host myserver` block it would silently apply to that host only. Verify
   with `ssh -G somehost | grep controlmaster` — expect `auto`.
2. ssh uses the **first** value it obtains per parameter, so any `Host` block
   above the Include keeps winning. If your host blocks pin
   `ServerAliveInterval 60`, they keep 60; delete those lines to inherit the
   faster default.

To undo everything, remove the `Host *` + `Include` lines from `~/.ssh/config`.

### When SSH still is not enough

Multiplexing and keepalives help SSH *recover*, but SSH is still TCP: a stalled
segment stalls your typing. Over persistently congested Wi-Fi or tethering,
[`mosh`](https://mosh.org) replaces the transport with UDP and echoes
keystrokes locally, so typing stays responsive through spikes and survives
suspend/resume and IP changes. Needs UDP 60000–61000 open to the server.

## Fix order

A radio problem is not fixed by tuning the client. In order of effect:

1. **Plug in Ethernet** — removes the contention entirely.
2. **Move to 5 GHz** (`wifi-band-prefer 5`) — if the signal supports it.
3. **Change the AP's 2.4 GHz channel** to the least crowded of 1/6/11, and turn
   off Wi-Fi Direct on printers and TVs that squat on your channel.
4. **Stop Bluetooth audio competing** — use a wired or USB output at the desk.

Tuning TCP sysctls is not on this list on purpose: it cannot create airtime
that the radio does not have.
