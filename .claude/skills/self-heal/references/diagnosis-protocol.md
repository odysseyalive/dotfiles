# Diagnosis Protocol

How to determine whether friction originated in the skill's own instructions.

## When to Run

Run at **natural task resolution** — when:
- The user signals the task is done ("thanks", "perfect", "that's it")
- The conversation reaches a clear stopping point
- The user moves to a new topic

Do NOT run mid-session. Wait for resolution.

## Step 1: Assess the Friction Log

If empty → no diagnosis needed. Stop.
If only Quality signals (low confidence) → skip diagnosis. Stop.
If at least one Explicit or Implicit signal → proceed to Step 2.

## Step 2: Spawn Root-Cause Analyst

Spawn with:
- The skill's full SKILL.md content
- The friction log (signal type, text, turn, hypothesis)
- The task summary
- The target skill's `eval-history.md` (if it exists)

Returns one of three verdicts:

**SKILL_CAUSED** — Names the exact section, quotes the instruction, explains the causal chain.
**REASONING_CAUSED** — Skill instructions were adequate; AI reasoned incorrectly. Stop.
**AMBIGUOUS** — Cannot determine with confidence. Stop.

## Step 2b: Check Self-Heal History

Check `.claude/skills/[skill-name]/self-heal-history.md`. If the same instruction was previously diagnosed and a patch was declined, do not re-propose the same change.

## Step 3: Spawn Patch Reviewer (only if SKILL_CAUSED)

Checks:
1. **Minimality** — Smallest change that fixes the root cause?
2. **Completeness** — Actually fixes the misrepresentation?
3. **Directive safety** — Touches any directive? → REJECT
4. **New ambiguity** — Introduces new unclear terms?

Returns: APPROVED or REJECTED with reason.

## Step 4: Present to User (only if APPROVED)

Natural, conversational tone:

```
I noticed I had some trouble with [brief description] earlier.
I think it came from how this skill describes [specific area].

**Current:**
[Exact quoted text]

**Proposed:**
[Exact quoted text]

Would you like me to update the skill?
```

## Step 5: Apply (only with explicit approval)

1. Read current SKILL.md fresh
2. Apply exact change — nothing more
3. Confirm: "Updated. The skill now says [new text]."
4. Append record to `self-heal-history.md`

## In Scope

| Content Type | In Scope? |
|-------------|-----------|
| Workflow step descriptions | YES |
| Context/framing statements | YES |
| Clarifying language and examples | YES |
| Scope descriptions | YES |
| Directives | **NEVER** |
| Reference file content | NO |
| Agent definitions | NO |
| Frontmatter | NO |
