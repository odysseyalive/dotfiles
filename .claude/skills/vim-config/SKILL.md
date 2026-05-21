---
name: vim-config
description: "Vim configuration with Vim-Plug plugin management, tag navigation, search, and Xdebug debugging for YADRLite."
allowed-tools: Read, Glob, Grep, Edit, Write
minimum-effort-level: high
---

# Vim Configuration

Vim configuration management with Vim-Plug, tag navigation, search, and Xdebug debugging for YADRLite.

---

## Vim Plugin Management

Uses **Vim-Plug** for plugin management:
- Install plugins: `:PlugInstall` (from Vim Normal mode)
- Update plugins: `:PlugUpdate`
- Settings are modular in `vim/settings/`

---

## Working with Vim

**Tag Management**:
- Build tags: `:MakeTags`
- Navigate: `,gt` (search tag), `,gi` (next in history), `,go` (back in history)

**Search**:
- Project search: `,ag` (Silver Searcher + Fzf)
- Directory search: `,ad`
- Current word: `,aw`
- Global search/replace: `:Gsearch foo` → `%s/search/replace/g` → `:Greplace` → `:wall`

**Xdebug Debugging**:
- Configure `.vimrc.local` per project (see README)
- Key mappings: `F5` (run), `F2` (step over), `F3` (step into), `F10` (breakpoint)

---

## Grounding

Before modifying Vim configuration:
1. Read `vim/vimrc` AND glob `vim/settings/*.vim` and read the file that most plausibly owns the setting being changed (filename keyword match against the setting domain — plugins.vim, search.vim, debug.vim, etc.).
2. Grep both reads for the specific setting / keybinding / plugin being changed. IF the symbol is not present in either file → STOP and report the search targets before adding new configuration; the user may be referring to a different file.
3. Read [reference.md](reference.md) § "File paths and key mappings" before quoting any path or keybinding back to the user, to avoid drift between this SKILL.md and the canonical mapping table.

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
5. IF diagnosis attributes friction to vim-config instructions → propose the surgical correction per the diagnosis-protocol's reporting format.
6. IF diagnosis attributes friction elsewhere → end normally without mentioning self-heal.

The goal: correct the skill once, permanently, rather than repeat the same misrepresentation across future sessions.
