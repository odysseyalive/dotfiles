---
name: yadrlite-setup
description: "YADRLite installation, tools, ASDF languages, migrations, update, uninstall, and Arch workstation setup commands."
allowed-tools: Read, Glob, Grep, Edit, Write, Bash
minimum-effort-level: high
---

# YADRLite Setup

Installation and lifecycle management for the YADRLite dotfiles package.

---

## Common Commands

- `zsh ~/.yadrlite/setup.zsh tools` — install Go tools and language servers
- `zsh ~/.yadrlite/setup.zsh --with-langs` — install all ASDF-managed languages
- `zsh ~/.yadrlite/setup.zsh --migrate` — apply rolling migrations in `setup/migrations/v*`
- `zsh ~/.yadrlite/setup.zsh update` — update dotfiles and components
- `zsh ~/.yadrlite/setup.zsh remove` — restore backups and uninstall

All original dotfiles are backed up to `~/.yadrlite/backup/` before installation.

---

## Grounding

Before suggesting install, update, or migration commands:
1. Read [reference.md](reference.md) § "Command catalog" in full BEFORE quoting any command form or flag to the user. Do not reconstruct command syntax from memory.
2. For any script invocation under `setup/`, Bash `ls setup/<target>` (or Glob `setup/<target>*`) and confirm the script file exists at that path. IF the script does not exist → STOP and report the missing target before suggesting the command.
3. State which reference section each suggested command came from (e.g., "Command quoted from reference.md § Migrations") so the catalog stays the source of truth.

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
5. IF diagnosis attributes friction to yadrlite-setup instructions → propose the surgical correction per the diagnosis-protocol's reporting format.
6. IF diagnosis attributes friction elsewhere → end normally without mentioning self-heal.

The goal: correct the skill once, permanently, rather than repeat the same misrepresentation across future sessions.
