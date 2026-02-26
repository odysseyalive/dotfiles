# Record Type Templates
<!-- Enforcement: HIGH — record structure enables cross-referencing and agent consultation -->

## Incident Record (INC)

**ID format:** `INC-YYYY-MM-DD-slug`

```markdown
# INC-YYYY-MM-DD-slug

**Status:** active | resolved | superseded
**Tags:** [domain tags, file paths, function names]
**Related:** [DEC-xxx, PAT-xxx, FLW-xxx, INC-xxx]

## What Happened

[Factual description of the incident. No blame. What was observed?]

## Timeline

| Time/Commit | Event |
|-------------|-------|
| [ref] | [what happened] |

## Root Cause

[Direct technical cause]

## Contributing Factors (Swiss Cheese Layers)

1. **[Layer]** — [How this factor contributed]
2. **[Layer]** — [How this factor contributed]

## Resolution

[What was done to fix it]

## Lessons Learned

> **"[Verbatim lesson, in the words of whoever identified it]"**

*— Captured YYYY-MM-DD, source: [conversation / commit / user]*

## Prevention

[What would prevent recurrence — link to DEC or PAT records if applicable]
```

## Decision Record (DEC)

**ID format:** `DEC-YYYY-MM-DD-slug`

```markdown
# DEC-YYYY-MM-DD-slug

**Status:** proposed | accepted | deprecated | superseded-by [DEC-xxx]
**Tags:** [domain tags, file paths, architectural area]
**Related:** [INC-xxx, PAT-xxx, FLW-xxx, DEC-xxx]

## Context

[What is the issue that we're seeing that motivates this decision?]

## Decision Drivers

- [Driver 1]
- [Driver 2]

## Options Considered

### Option A: [Name]

- Good, because [argument]
- Bad, because [argument]

### Option B: [Name]

- Good, because [argument]
- Bad, because [argument]

## Decision

Chosen option: **[Option X]**, because [justification].

> **"[Verbatim rationale from the person who made the call]"**

*— Captured YYYY-MM-DD, source: [conversation / commit / user]*

## Consequences

- [Consequence 1 — positive or negative]
- [Consequence 2]

## Confirmation Criteria

[How will we know this decision was right? What would trigger reconsideration?]
```

## Pattern Record (PAT)

**ID format:** `PAT-YYYY-MM-DD-slug`

```markdown
# PAT-YYYY-MM-DD-slug

**Status:** active | deprecated | under-review
**Tags:** [domain tags, file paths, language/framework]
**Related:** [INC-xxx, DEC-xxx, FLW-xxx, PAT-xxx]

## Pattern

[What is the reusable knowledge?]

## Evidence

1. **[Evidence]** — [source/date]
2. **[Evidence]** — [source/date]

## Counter-Evidence

1. **[Counter-evidence]** — [source/date]
2. **[Counter-evidence]** — [source/date]

## Applicability

- **When to use:** [conditions where this pattern applies]
- **When NOT to use:** [conditions where this pattern fails or misleads]

## Confidence

[HIGH / MEDIUM / LOW] — Based on evidence-to-counter-evidence ratio and recency.
```

## Flow Record (FLW)

**ID format:** `FLW-YYYY-MM-DD-slug`

```markdown
# FLW-YYYY-MM-DD-slug

**Status:** active | outdated | superseded-by [FLW-xxx]
**Tags:** [domain tags, user action, system component]
**Related:** [INC-xxx, DEC-xxx, PAT-xxx, FLW-xxx]

## Flow Description

[What user action or system process does this flow capture?]

## Steps

| Step | Action | Code Path | Notes |
|------|--------|-----------|-------|
| 1 | [what happens] | `file:line` | [relevant detail] |
| 2 | [what happens] | `file:line` | |

## Environmental Conditions

- [Condition that must be true for this flow to occur]

## Edge Cases

- [Edge case 1]
- [Edge case 2]
```

## Status Lifecycle

```
proposed → active → [resolved | deprecated | superseded-by REF]
                  → under-review → active (re-confirmed)
```
