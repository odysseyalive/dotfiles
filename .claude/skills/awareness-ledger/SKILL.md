---
name: awareness-ledger
description: "Institutional memory for your project. Commands: record, consult, review. Usage: /awareness-ledger [command] [args]"
allowed-tools: Read, Glob, Grep, Write, Edit, Task, TaskCreate, TaskUpdate, TaskList, TaskGet
---

# Awareness Ledger

Institutional memory for the YADRLite dotfiles project. Captures incidents, decisions, patterns, and flows so diagnostic findings and architectural decisions persist across sessions.

## Usage

```
/awareness-ledger [command] [args]
```

### Modes

| Mode | Command | Description |
|------|---------|-------------|
| `record` | `/awareness-ledger record [type]` | Create a new ledger record (INC/DEC/PAT/FLW) |
| `consult` | `/awareness-ledger consult [topic]` | Query ledger for relevant records, spawn agents |
| `review` | `/awareness-ledger review` | Audit ledger health: stale entries, missing links, stats |

Default mode is `consult` if not specified.

---

## Directives

### Auto-Consultation (READ)

During research and planning — before formulating any plan, recommendation, or
code change proposal — automatically consult the ledger:

1. **Index scan** — Read `ledger/index.md` and match tags against the files,
   directories, and components under discussion. This is free — the index is
   small. Do this as part of your initial research, alongside reading source
   files.
2. **Record review** — If matching records exist, read the full record files.
   Incorporate warnings, known failure modes, and relevant decisions into your
   thinking before presenting any plan to the user. This is cheap — records
   are short.
3. **Agent escalation** — If high-risk overlap is detected (matching INC records
   with active status, or multiple record types matching the same change area),
   spawn consultation agents proportionally per the agent table in
   `references/consultation-protocol.md`. Present agent findings as part of
   your recommendation. This is expensive — only when warranted.

The consultation must happen during planning, not at edit time. By the time
code is being written, the plan has already been presented and approved. The
ledger's value is in shaping the plan itself — surfacing past failures,
challenging assumptions, and providing historical context that changes what
you recommend.

Skip auto-consultation for:
- Changes to `.claude/` infrastructure files
- Trivial edits (typos, formatting, comments)
- Areas with no tag overlap in the index

### Auto-Capture Suggestion (WRITE)

When the current conversation produces institutional knowledge, suggest recording it **after** resolving the immediate issue. Never interrupt active problem-solving to suggest capture.

Automatically suggest capture when you encounter:
- **Bug investigation** with timeline, root cause analysis, or contributing factors → INC record
- **Architectural decisions** with trade-offs discussed and option chosen → DEC record
- **Recurring patterns** observed across multiple instances or confirmed by evidence → PAT record
- **User/system flows** traced step-by-step with code paths identified → FLW record

Capture suggestions are always user-confirmed. Present the suggestion with:
- Suggested record type and ID
- Key content to capture (quoted from conversation)
- One-line confirmation prompt: "Record this in the awareness ledger? (confirm/skip)"

---

## Workflow: Record

1. Read `references/templates.md` for the requested record type template
2. Gather facts from the current conversation context
3. Fill the template with factual content (no speculation)
4. Write the record to `ledger/[type]/[ID].md`
5. Update `ledger/index.md` with the new entry
6. Report: "Recorded [ID] in the awareness ledger."

## Workflow: Consult

1. Read `ledger/index.md` to find matching records by tag
2. If matches found, read full record files
3. Assess risk level:
   - Low overlap → report relevant records inline
   - High overlap → spawn agents per `references/consultation-protocol.md`
4. Synthesize findings into the consultation briefing format
5. If conversation contains capturable knowledge, append a capture suggestion

## Workflow: Review

1. Scan all record files in `ledger/`
2. Check for: stale entries (>90 days without review), missing cross-references, orphan tags
3. Report statistics: record counts by type and status
4. Flag issues and suggest fixes

---

## Grounding

Before creating or consulting any record:
1. Read the relevant file from `references/`
2. State: "I will use [TEMPLATE/PROTOCOL] from references/[file] under [SECTION]"

Reference files:
- [references/templates.md](references/templates.md) — Record type templates (INC/DEC/PAT/FLW)
- [references/consultation-protocol.md](references/consultation-protocol.md) — Agent spawning rules, proportional overhead
- [references/capture-triggers.md](references/capture-triggers.md) — Conversation signal patterns

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
