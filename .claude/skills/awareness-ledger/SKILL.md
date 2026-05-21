---
name: awareness-ledger
description: "Institutional memory for your project. Commands: record, consult, review. Usage: /awareness-ledger [command] [args]"
allowed-tools: Read, Glob, Grep, Write, Edit, Task, TaskCreate, TaskUpdate, TaskList, TaskGet
minimum-effort-level: high
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

<!-- ENFORCEMENT ANNOTATION — auto-generated for Opus 4.7+ literal execution -->
<!-- Source directive: "Auto-Consultation: index scan + record review + agent escalation during research and planning, before any plan is formed." -->
CHECKPOINT — Planning-Phase Consultation Gate:
1. Detect planning context: the user has asked for a plan, recommendation, design, or code change — and no plan has been presented yet this turn. IF the user is asking for a one-line trivial edit (typo, formatting, comment-only) → SKIP this CHECKPOINT.
2. Read `.claude/skills/awareness-ledger/ledger/index.md`. Extract every tag and the directory/file paths it cross-references.
3. Build a current-scope set: list every file, directory, and component named in the user's request and in your in-progress research notes.
4. Intersect step 2 tags with step 3 scope. IF intersection is empty → state "No ledger overlap with current scope" and CONTINUE without consultation.
5. IF intersection is non-empty → read every matching record file in full BEFORE forming the plan. Surface their warnings, known failure modes, and decisions inline in your recommendation.
6. Compute risk:
   - HIGH RISK if: any matching INC record has `status: active` OR the intersection produced ≥2 record types (INC+DEC, INC+PAT, etc.) covering the same change area.
   - Otherwise LOW RISK.
7. IF HIGH RISK → read `references/consultation-protocol.md` § agent table and spawn the proportional agent set listed there. Include agent findings in the recommendation.
8. IF LOW RISK → cite the relevant records inline without spawning agents.
9. IF this CHECKPOINT is reached AFTER a plan has already been presented to the user → STOP and report: "Consultation gate was skipped earlier — the plan was formed without ledger context. Re-run planning with consultation applied." Consultation must shape the plan, not validate it after the fact.
<!-- END ENFORCEMENT ANNOTATION -->

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

<!-- ENFORCEMENT ANNOTATION — auto-generated for Opus 4.7+ literal execution -->
<!-- Source directive: "Auto-Capture Suggestion: suggest recording after resolving the immediate issue. Never interrupt active problem-solving. Detect INC/DEC/PAT/FLW knowledge as it emerges." -->
CHECKPOINT — Capture Suggestion Gate:
1. Maintain a per-turn "capture signal" ledger as you work. Append an entry every time ANY of these is observed:
   - The user describes a bug investigation that includes timeline, root cause, or contributing factors → candidate INC.
   - The user discusses ≥2 implementation options and chooses one with trade-offs articulated → candidate DEC.
   - You observe the same pattern across ≥2 prior records, prior commits, or prior conversation turns → candidate PAT.
   - The user traces a user-facing or system flow step-by-step with code paths or component boundaries identified → candidate FLW.
2. Gate the suggestion on task resolution: the immediate fix/answer/code-change has been delivered AND the user has either signaled done OR moved to a new topic. IF active problem-solving is still in progress (the bug is not yet fixed, the question not yet answered) → DO NOT surface the suggestion. Hold it.
3. When the gate opens, for each entry in the per-turn capture ledger:
   a. Compose one suggestion using the format in `### Auto-Capture Suggestion (WRITE)` (type + ID + quoted content + confirmation prompt).
   b. Surface ALL pending suggestions in a single batched message at the end of the resolved turn — do not interleave with other output.
4. IF the user replies "confirm" → run `/awareness-ledger record [type]` for that suggestion. IF "skip" → discard and do not re-surface in the same session.
5. IF the per-turn capture ledger is empty when the gate opens → say nothing about capture. Do not surface "no captures needed" notices.
<!-- END ENFORCEMENT ANNOTATION -->

---

## Workflow: Record

1. Read `references/templates.md` for the requested record type template
2. Gather facts from the current conversation context
3. Fill the template with factual content (no speculation)
4. Write the record to `ledger/[type]/[ID].md`
5. Update `ledger/index.md` with the new entry
6. Report: "Recorded [ID] in the awareness ledger."

## Workflow: Consult

1. Read `ledger/index.md` to find matching records by tag.
2. IF zero matches → report "No ledger overlap with current scope" and STOP this workflow.
3. IF ≥1 match → read every matching record file in full before proceeding.
4. Assess risk level:
   - HIGH overlap = any matched INC record has `status: active` OR ≥2 record types matched on the same change area. Spawn agents per `references/consultation-protocol.md` § "Agent table" and include their findings in the briefing.
   - LOW overlap = neither HIGH condition met. Report relevant records inline without spawning agents.
5. Synthesize findings into the consultation briefing format described in `references/consultation-protocol.md` § "Briefing format".
6. Run the Auto-Capture Suggestion CHECKPOINT (above, under `## Directives`). IF its gate opens and the per-turn capture ledger is non-empty → append the batched suggestion to the briefing.

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

Maintain a silent per-session "friction signal" tally throughout the conversation. Do not mention the tally to the user. Do not interrupt active work to address it.

CHECKPOINT — Friction Observation (runs continuously, silent):
1. After every assistant turn, scan the most recent user message for these signals:
   - The user issues a correction beginning with "actually", "no", "wait", "instead", "I meant".
   - The user reformulates a request that was already given this session (paraphrases the same instruction).
   - The user asks a clarifying question whose answer was supposed to be supplied by THIS skill's instructions.
   - The user says the result misrepresents what they asked for.
2. For each signal observed, append an entry: `{ turn_id, signal_type, quoted_user_text }` to the in-memory friction tally. Do not log to disk. Do not surface to user.
3. Detect "natural task resolution" using ALL of: (a) the immediate task delivered, (b) the user said "thanks", "done", "ok", "great", or moved to a new topic, AND (c) no in-flight tool calls remain.
4. AT natural task resolution:
   - IF the friction tally is empty → end the session normally. Do not mention self-heal.
   - IF the friction tally is non-empty → Read `.claude/skills/self-heal/references/diagnosis-protocol.md` and execute its steps literally with the tally as input.
5. IF diagnosis concludes the friction came from skill instructions in awareness-ledger → propose the surgical correction to the user per the diagnosis-protocol's reporting format.
6. IF diagnosis concludes the friction came from elsewhere (user clarification, ambient context, unrelated tool failure) → end the session normally without mentioning self-heal.

The goal is efficiency: correct the skill once, permanently, rather than repeat the same misrepresentation across future sessions.

---

<!-- ROUTE-EMBED START — auto-generated by /skill-builder route embed; safe to replace -->
<!-- origin: skill-builder | modifiable: true -->
## Route Consultation

<!-- ENFORCEMENT ANNOTATION — Opus 4.7+ literal-execution gate -->
<!-- Source: /skill-builder route embed. Closes the dispatch-bypass gap: a /route announcement is not a substitute for a Skill tool call, and consulting /route does not license the caller to execute the target skill's procedure manually. -->
CHECKPOINT — Skill Routing Gate:
1. During this skill's workflow, IF a step requires research, web search, lookup, analysis, or any follow-up NOT already covered by an explicit tool or skill name → STOP before improvising.
2. Already-named exception. IF the current workflow step EXPLICITLY names tools or skills (e.g., "use Read", "spawn the X agent", "invoke /[skill]") → execute as written. Do NOT consult `/route` for steps already determined. Skip to your skill's next step.
3. Otherwise → invoke `/route <task description>` via the Skill tool. `/route` reads its index and either returns a target skill or reports "no match."
4. Dispatch Required. IF `/route` returns a target → the IMMEDIATE next tool call MUST be `Skill(skill=<target>, args=<derived>)`. Do NOT execute the target skill's procedure steps yourself with raw Edit / Write / Bash / Task / Agent / Read calls. The target skill's gates (asset backups, agent panels, exit tests, cascade steps, frontend-design reviews, immutable-directive checks) ONLY fire when its SKILL.md is loaded via Skill — running steps manually silently skips every gate.
5. Bypass detection. IF a Skill dispatch was just announced (yours or `/route`'s) and the next tool call is non-Skill work germane to the routed task → STOP. Print verbatim: "Route dispatch announced but Skill call skipped. Invoking Skill tool now." then issue the Skill call.
6. No-match path. IF `/route` returns "no match" → proceed with the most direct, manual approach. Document the gap; the user may want to register a skill.
7. Follow-ups loop back. After the dispatched skill returns, IF further follow-up is needed → consult `/route` again via Skill. Do not freelance the next step.
8. Auto-mode override clause. Auto-mode ("execute immediately", "prefer action over planning") does NOT override clauses 4–5. Auto-mode chooses WHAT to do; once a routing decision is on the table, the only valid next action is Skill invocation.
<!-- END ENFORCEMENT ANNOTATION -->
<!-- /origin -->
<!-- ROUTE-EMBED END -->
