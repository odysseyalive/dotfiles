---
name: editor-tools
description: "Tmux and Ranger configuration, plugin management, and workflows for YADRLite."
allowed-tools: Read, Glob, Grep, Edit, Write
minimum-effort-level: high
---

# Editor Tools

Tmux and Ranger configuration management for YADRLite.

---

## Tmux Plugin Management

- Plugins cloned to `~/.yadrlite/tmux/plugin/` during setup
- Leader key: `Ctrl-a`
- tmux-resurrect for session persistence: `Ctrl-a Ctrl-s` (save), `Ctrl-a Ctrl-r` (restore)

---

## Working with Ranger

Install configuration:
```bash
mkdir -p ~/.config/ranger && rsync -azhLP ~/.yadrlite/workstation/ranger/ ~/.config/ranger
```

Requires: `w3m` (image previews), `atool` (archive handling)

---

## Grounding

Before modifying Tmux or Ranger configuration:
1. Read the relevant config file (`tmux/tmux.conf` for Tmux changes; the files under `workstation/ranger/` for Ranger changes).
2. Grep the file for the setting key BEFORE issuing an Edit. IF the key is not present in the file → STOP and report which file was searched and that the setting must be added explicitly rather than modified.
3. Read [reference.md](reference.md) § "File paths and key mappings" before quoting any file path or key binding back to the user, to avoid drift between this SKILL.md and the canonical mapping table.

---

## Self-Heal Observer

Maintain a silent per-session "friction signal" tally throughout the conversation. Do not mention the tally to the user. Do not interrupt active work to address it.

CHECKPOINT — Friction Observation (runs continuously, silent):
1. After every assistant turn, scan the most recent user message for these signals:
   - The user issues a correction beginning with "actually", "no", "wait", "instead", "I meant".
   - The user reformulates a request that was already given this session.
   - The user asks a clarifying question whose answer was supposed to be supplied by THIS skill's instructions.
   - The user states the result misrepresents what they asked for.
2. For each signal observed, append `{ turn_id, signal_type, quoted_user_text }` to the in-memory tally. Do not log to disk.
3. Detect natural task resolution = (a) immediate task delivered AND (b) user said "thanks"/"done"/"ok"/"great" OR moved to a new topic AND (c) no in-flight tool calls.
4. AT natural task resolution:
   - IF tally is empty → end normally. Do not mention self-heal.
   - IF tally is non-empty → Read `.claude/skills/self-heal/references/diagnosis-protocol.md` and execute its steps literally with the tally as input.
5. IF diagnosis attributes friction to editor-tools instructions → propose the surgical correction per the diagnosis-protocol's reporting format.
6. IF diagnosis attributes friction elsewhere → end normally without mentioning self-heal.

The goal: correct the skill once, permanently, rather than repeat the same misrepresentation across future sessions.
