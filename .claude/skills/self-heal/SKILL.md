---
name: self-heal
description: "Ambient friction detection and surgical skill correction. Embeds into all skills. Triggers at task resolution when skill instructions caused friction."
allowed-tools: Read, Write, Edit, Task, TaskCreate, TaskUpdate
minimum-effort-level: high
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

<!-- ENFORCEMENT ANNOTATION — auto-generated for Opus 4.7+ literal execution -->
<!-- Source directive: "Updates to skills must be surgical -- one specific instruction, smallest possible change, nothing more." -->
CHECKPOINT — Surgical-Update Gate:
1. Before composing any patch, identify the ONE failing instruction that produced the friction. Quote it verbatim. IF the friction maps to ≥2 distinct instructions → STOP. Patch the highest-impact instruction only; record the rest in `self-heal-history.md` for a future pass.
2. The patch must touch exactly ONE contiguous span of text. Count the lines BEFORE and AFTER. IF the diff modifies non-contiguous spans (multiple hunks) → STOP. Split into separate patches and apply only the first; defer the others.
3. The patch must not add new instructions, sections, or behavioral capabilities not already present in the failing instruction's scope. IF the proposed change introduces a new rule, capability, or workflow step → STOP. Reduce to a wording fix only.
4. The diff line count (changed + added) must be ≤ 5 lines for a single-instruction patch. IF the diff is larger → STOP. Either (a) the instruction is multi-faceted (split per step 1), or (b) the patch is over-scoped (reduce per step 3).
5. IF all four gates pass → present the before/after diff and the surgical-update justification to the user per the second sacred directive ("Nothing is written... without explicit user approval and a visible before/after diff"). Wait for explicit "approve" or equivalent before issuing Edit.
6. IF any gate fails → report which gate failed and the rule it violated. Do not apply the patch.
<!-- END ENFORCEMENT ANNOTATION -->

---

## Review Command

When invoked as `/self-heal review`:

1. Glob `.claude/skills/*/SKILL.md` and grep each for the literal heading `## Self-Heal Observer`. Skills containing this heading are "embedded"; the rest are "missing observer".
2. For every embedded skill, read `.claude/skills/<skill>/self-heal-history.md`. IF the file does not exist → record `events=0, applied=0, declined=0` and continue (do not infer history from elsewhere). IF the file exists → parse it and count rows where `outcome=applied` and `outcome=declined`.
3. Emit a per-skill table with columns: skill name, observer embedded (yes/no), friction events logged, patches applied, patches declined.
4. For each skill in the "missing observer" set, present a single batched offer: "Embed self-heal observer in: [list]. Approve which?" Do not embed without explicit per-skill user approval.

---

## Grounding

Before executing any diagnosis or update, read the protocol files BEFORE acting on them:

1. For friction signal classification → Read [references/friction-signals.md](references/friction-signals.md) in full before tagging signals.
2. For root-cause tracing → Read [references/diagnosis-protocol.md](references/diagnosis-protocol.md) in full before proposing any attribution.
3. For diff construction and approval workflow → Read [references/update-protocol.md](references/update-protocol.md) in full before issuing Edit.

State out loud which protocol section you are applying before each action, naming the file and section (e.g., "Applying diagnosis-protocol.md § Root cause tracing"). This statement is not optional — it is the grounding ledger that proves the protocol was read.

Reference files:
- [references/friction-signals.md](references/friction-signals.md) — Complete friction signal taxonomy
- [references/diagnosis-protocol.md](references/diagnosis-protocol.md) — Root cause tracing procedure
- [references/update-protocol.md](references/update-protocol.md) — Diff construction and approval workflow
