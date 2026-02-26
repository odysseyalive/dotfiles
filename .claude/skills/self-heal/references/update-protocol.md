# Update Protocol

How to construct the before/after diff and apply approved changes.

## Diff Construction Rules

1. **Exact** — Quote the current instruction verbatim
2. **Scoped** — Show only the changed portion, plus one sentence of context on each side
3. **Honest** — If the change is more than a sentence, reconsider minimality
4. **Readable** — Plain text diff. No code diff syntax.

If the diff requires showing more than 3-4 lines of change, the patch is not surgical enough.

## Approval Language

The proposal must:
- Identify what went wrong in plain language
- Show the diff clearly labeled "Current" and "Proposed"
- End with a simple yes/no question

The proposal must NOT:
- Use system language ("PATCH APPROVED", "DIAGNOSIS RESULT")
- Apologize excessively
- Explain the diagnostic process
- Add undermining caveats

## Application Rules

1. Read the current SKILL.md fresh
2. Locate the exact string from the "Current" portion
3. Replace with the "Proposed" text — nothing else
4. Verify the change by re-reading
5. Report confirmation in one sentence
6. Append a record to `self-heal-history.md`

If the exact string cannot be located:
- Report: "The skill file seems to have changed. Would you like me to show the diff again?"
- Do not apply blindly
