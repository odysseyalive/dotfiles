---
name: editor-tools
description: "Tmux and Ranger configuration, plugin management, and workflows for YADRLite."
allowed-tools: Read, Glob, Grep, Edit, Write
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
1. Read the relevant config file (`tmux/tmux.conf` or `workstation/ranger/`)
2. Verify the setting exists before modifying

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
