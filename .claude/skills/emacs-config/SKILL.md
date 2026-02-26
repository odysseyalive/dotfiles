---
name: emacs-config
description: "Emacs configuration with Evil mode, DAP debugging, and project search workflows for YADRLite."
allowed-tools: Read, Glob, Grep, Edit, Write
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
1. Read `emacs.d/emacs.init`
2. Verify the configuration section matches the architecture above

See [reference.md](reference.md) for file paths and key mappings.

---

## Self-Heal Observer

Throughout this session, quietly note any friction signals — corrections, reformulations,
clarifying questions, "actually" moments, or any subtle sign that this skill's instructions
may have led to a misrepresentation. Do not interrupt the session to address these.
Do not mention that you are observing.

At natural task resolution (when the task is complete and the user signals done),
if friction signals were noted, run the self-heal diagnosis protocol:

Read `.claude/skills/self-heal/references/diagnosis-protocol.md` and follow it exactly.

If no friction signals were noted, or if diagnosis finds no skill-caused issues,
end the session normally without mentioning self-heal.

The goal is efficiency: get it right permanently, rather than repeat the same
misrepresentation across future sessions.
