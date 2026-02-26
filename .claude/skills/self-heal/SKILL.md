---
name: self-heal
description: "Ambient friction detection and surgical skill correction. Embeds into all skills. Triggers at task resolution when skill instructions caused friction."
allowed-tools: Read, Write, Edit, Task, TaskCreate, TaskUpdate
---

# Self-Heal

Quiet companion skill. Watches for friction caused by skill instructions during live sessions. At task resolution, diagnoses root cause and proposes surgical fixes with user approval.

**This skill does not run on demand.** It is embedded into other skills as an observer. When it detects a fixable source of friction, it surfaces automatically.

---

## Directives

> **"Updates to skills must be surgical -- one specific instruction, smallest possible change, nothing more."**

*— Added 2026-02-26, source: system directive*

> **"Nothing is written to a skill file without explicit user approval and a visible before/after diff."**

*— Added 2026-02-26, source: system directive*

> **"Self-heal never modifies directives. Directives are sacred. Only workflow instructions, context descriptions, and clarifying language are in scope."**

*— Added 2026-02-26, source: system directive*

---

## Review Command

When invoked as `/self-heal review`:

1. List all skills that have the self-heal observer embedded
2. Show count of friction events logged per skill (read each skill's `self-heal-history.md` if it exists)
3. Show count of patches applied vs. declined per skill
4. Flag skills missing the observer — offer to embed

---

## Grounding

Before executing any diagnosis or update:
1. Read the relevant file from `references/`
2. State: "I will use [PROTOCOL] from references/[file] under [SECTION]"

Reference files:
- [references/friction-signals.md](references/friction-signals.md) — Complete friction signal taxonomy
- [references/diagnosis-protocol.md](references/diagnosis-protocol.md) — Root cause tracing procedure
- [references/update-protocol.md](references/update-protocol.md) — Diff construction and approval workflow
