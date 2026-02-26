# Consultation Protocol

## Triage Rules

When consulting the ledger, match current context (file paths, components, domains) against the index tags.

## Proportional Agent Spawning

| Match Scope | Agents Spawned | Rationale |
|-------------|----------------|-----------|
| No records match | None | Zero overhead until records exist |
| Only incidents/flows match | Regression Hunter only | Single relevant perspective |
| Only decisions/patterns match | Skeptic only | Single relevant perspective |
| Risk/failure language detected | Premortem Analyst only | Targeted premortem |
| Multiple record types match | All three agents (full panel) | Cross-referencing needed |

## Synthesis Rules

- **All agents agree** → HIGH confidence warning
- **Agents disagree** → The disagreement IS the signal. Present all perspectives.
- **Single agent with finding** → MEDIUM confidence, present as consideration

## Consultation Briefing Format

```markdown
## Ledger Consultation

### Warnings (HIGH confidence — agents agree)
- **[Warning]** — [record reference] — [explanation]

### Considerations (agents disagree — investigate)
- **[Topic]**
  - Regression Hunter: [finding]
  - Skeptic: [finding]
  - Premortem Analyst: [finding]

### Context (relevant records, no warnings)
- [ID] — [why it's relevant]

### Capture Opportunity
The current conversation contains knowledge not yet in the ledger:
- **Suggested type:** [INC/DEC/PAT/FLW]
- **Suggested ID:** [auto-generated]
- **Source material:** [quote from conversation]
```
