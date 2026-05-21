---
name: emacs-config
description: "Emacs configuration with Evil mode, DAP debugging, and project search workflows for YADRLite."
allowed-tools: Read, Glob, Grep, Edit, Write
minimum-effort-level: high
---

# Emacs Configuration

Emacs configuration management with Evil mode, debugging, and project search workflows for YADRLite.

---

## Configuration

- Uses Evil mode for Vim keybindings
- Toggle Evil/Emacs mode: `Ctrl-z`
- Leader key: `,` (General.el plugin enables concurrent typing, no need to hold comma)
- Includes DAP-mode for debugging with vscode-php-debug extension (unzipped to `~/.yadrlite/emacs.d/.extension/`)

---

## Working with Emacs

**Install standalone** (without full YADRLite):
```bash
curl https://raw.githubusercontent.com/odysseyalive/dotfiles/master/emacs.d/emacs.init > ~/.emacs
```

**Debugging**:
- `,dd`: Start debugger
- `,db`: Toggle breakpoint
- `,dc`: Continue
- `,di`: Step into
- `,dp`: Install vscode-php-debug plugin

**Project Search**:
- `,ps`: Search in current project
- `,pd`: Search in directory
- `,/`: Search current project
- `,pf`: Fuzzy file finder (CtrlP equivalent)

---

## Grounding

Before modifying Emacs configuration:
1. Read `emacs.d/emacs.init` in full.
2. Grep the file for the specific setting/keybinding/package being changed. IF the symbol is not present → STOP and report the search target before adding new configuration; the user may be referring to a different file.
3. Read [reference.md](reference.md) § "File paths and key mappings" before quoting any path or keybinding back to the user, to avoid drift between this SKILL.md and the canonical mapping table.

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
5. IF diagnosis attributes friction to emacs-config instructions → propose the surgical correction per the diagnosis-protocol's reporting format.
6. IF diagnosis attributes friction elsewhere → end normally without mentioning self-heal.

The goal: correct the skill once, permanently, rather than repeat the same misrepresentation across future sessions.
