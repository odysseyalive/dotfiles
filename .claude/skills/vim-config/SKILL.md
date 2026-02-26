---
name: vim-config
description: "Vim configuration with Vim-Plug plugin management, tag navigation, search, and Xdebug debugging for YADRLite."
allowed-tools: Read, Glob, Grep, Edit, Write
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
1. Read `vim/vimrc` and the relevant file in `vim/settings/`
2. Verify the setting exists before modifying

See [reference.md](reference.md) for file paths and key mappings.

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
