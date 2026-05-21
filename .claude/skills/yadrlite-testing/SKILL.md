---
name: yadrlite-testing
description: "YADRLite test suite: Makefile targets, ShellSpec BDD tests, ShellCheck, and zsh syntax linting."
allowed-tools: Read, Glob, Grep, Edit, Write, Bash
minimum-effort-level: high
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
1. Read the repo-root `Makefile` in full BEFORE recommending or invoking any make target. State which target you intend to use and quote its recipe back from the Makefile, to confirm behavior matches expectation.
2. Read [reference.md](reference.md) § "Tool stack" before naming any test tool (ShellSpec, ShellCheck, zsh syntax check, etc.) — invocation forms differ between tools and reconstruction from memory drifts.
3. For any new test, Bash `ls spec/` (or the canonical spec directory named in reference.md) and place the file alongside existing specs. IF the directory does not exist → STOP and read reference.md § "Test framework details" before creating it.

---

## Self-Heal Observer

Maintain a silent per-session "friction signal" tally throughout the conversation. Do not mention the tally to the user. Do not interrupt active work to address it.

CHECKPOINT — Friction Observation (runs continuously, silent):
1. After every assistant turn, scan the most recent user message for these signals:
   - The user issues a correction beginning with "actually", "no", "wait", "instead", "I meant".
   - The user reformulates a request that was already given this session.
   - The user asks a clarifying question whose answer was supposed to be supplied by THIS skill's instructions.
   - The user states the result misrepresents what they asked for.
2. For each signal observed, append `{ turn_id, signal_type, quoted_user_text }` to the in-memory tally. Do not log to disk.
3. Detect natural task resolution = (a) immediate task delivered AND (b) user said "thanks"/"done"/"ok"/"great" OR moved to a new topic AND (c) no in-flight tool calls.
4. AT natural task resolution:
   - IF tally is empty → end normally. Do not mention self-heal.
   - IF tally is non-empty → Read `.claude/skills/self-heal/references/diagnosis-protocol.md` and execute its steps literally with the tally as input.
5. IF diagnosis attributes friction to yadrlite-testing instructions → propose the surgical correction per the diagnosis-protocol's reporting format.
6. IF diagnosis attributes friction elsewhere → end normally without mentioning self-heal.

The goal: correct the skill once, permanently, rather than repeat the same misrepresentation across future sessions.
