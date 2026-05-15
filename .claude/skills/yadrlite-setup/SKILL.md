---
name: yadrlite-setup
description: "YADRLite installation, tools, ASDF languages, migrations, update, uninstall, and Arch workstation setup commands."
allowed-tools: Read, Glob, Grep, Edit, Write, Bash
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
1. Read `reference.md` for exact command forms, flag variants, and workstation installers
2. Verify the target script exists in `setup/` before invoking it

See [reference.md](reference.md) for full command catalog.

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
