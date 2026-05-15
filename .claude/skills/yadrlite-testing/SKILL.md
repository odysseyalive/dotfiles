---
name: yadrlite-testing
description: "YADRLite test suite: Makefile targets, ShellSpec BDD tests, ShellCheck, and zsh syntax linting."
allowed-tools: Read, Glob, Grep, Edit, Write, Bash
---

# YADRLite Testing

Test and linting workflows for YADRLite's shell scripts (`install.sh`, `setup/*`).

---

## Common Commands

- `make help` — list all available Make targets
- `make check` — run the complete test suite and linter

CI runs the same targets across Ubuntu and macOS via `.github/workflows/ci.yml`.

---

## Grounding

Before editing shell scripts or adding tests:
1. Read the Makefile to confirm target behavior
2. Read `reference.md` for the tool stack and how each tool is invoked

See [reference.md](reference.md) for test framework details and CI wiring.

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
