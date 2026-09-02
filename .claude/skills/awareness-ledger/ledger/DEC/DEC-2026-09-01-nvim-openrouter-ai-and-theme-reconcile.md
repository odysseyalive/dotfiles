# DEC-2026-09-01-nvim-openrouter-ai-and-theme-reconcile

**Status:** accepted
**Tags:** workstation/nvim, lazyvim, codecompanion, openrouter, copilot, ai-completion, theme.lua, kitty-themes.nvim, config-reconciliation
**Related:** [INC-2026-08-17-omarchy4-conf-to-lua-migration](../INC/INC-2026-08-17-omarchy4-conf-to-lua-migration.md)

## Context

`workstation/nvim` (the LazyVim template in the repo) had drifted from the live
`~/.config/nvim` since the Omarchy migration: the live machine gained Omarchy
theme hot-reload, an OSC52/Wayland `remote_clipboard.lua`, org agenda config,
snacks/news tweaks, and the neo-tree extra, while the repo still carried a
static `kitty-themes.lua` and some keymaps the live machine had dropped. The AI
tooling was GitHub Copilot for both inline completion and chat (CopilotChat).
The user asked whether OpenRouter could give better chat/agentic capability.

## Decision Drivers

- Inline completion fires on nearly every keystroke → cost/latency sensitive.
- Chat/agentic work benefits from frontier model choice (Claude, GPT, …).
- The repo must stay installable on non-Omarchy machines *and* coexist with the
  Omarchy-managed theme symlink without the earlier double-declaration breakage.
- Reproducible installs (plugin version pinning).

## Options Considered

### Option A: All-OpenRouter (minuet-ai for inline + CodeCompanion for chat)

- Good, because single provider, full model choice everywhere.
- Bad, because per-token inline completion is costlier and higher-latency than
  Copilot's flat-rate dedicated FIM.

### Option B: Hybrid — keep Copilot inline, add CodeCompanion+OpenRouter for chat

- Good, because flat-rate cheap/fast inline stays; chat gets frontier models via
  one OpenRouter key; CodeCompanion ships a built-in `openrouter` adapter.
- Bad, because two AI systems to understand.

### Option C: Stay on Copilot for everything

- Good, because zero change.
- Bad, because CopilotChat can't reach non-Copilot models.

## Decision

Chosen option: **Option B (hybrid)**. Added `codecompanion.lua` (chat/inline/cmd
→ built-in `openrouter` adapter, default `anthropic/claude-sonnet-4.5`, key from
`$OPENROUTER_API_KEY`), retired CopilotChat (dropped the `ai.copilot-chat` extra
+ `copilot-chat.lua`), and kept Copilot inline (`ai.copilot` + blink-copilot).

Reconciliation: brought all live improvements into the repo; committed
`lazy-lock.json` + `.gitignore`; and **renamed `kitty-themes.lua` → `theme.lua`**.
`theme.lua` still declares `odysseyalive/kitty-themes.nvim` + `colorscheme
"SeaShells"`, but its filename matches what Omarchy overrides with a symlink to
`~/.local/state/omarchy/current/theme/neovim.lua` — so on Omarchy the symlink
wins (no double declaration, the earlier "complication"), and off Omarchy the
static file is the fallback that keeps SeaShells.

> **"push both and record in the ledger"** — user, confirming the hybrid AI stack
> + reconciliation after review.

*— Captured 2026-09-01, source: conversation + commit 16c948a*

## Consequences

- Chat/agentic now model-agnostic via OpenRouter; inline unchanged and flat-rate.
- Keymaps `,aa` `,ac` `,ai` (+ visual `ga`) now belong to CodeCompanion.
- Repo and live are byte-identical except `theme.lua` (repo file vs Omarchy symlink).
- `/lazyvim` skill docs corrected (install path `workstation/nvim`; plugin tables).
- Requires `$OPENROUTER_API_KEY` in the environment (already present on this box).

## Confirmation Criteria

Right if chat quality improves without inline cost/latency regressions.
Reconsider if: OpenRouter inline (minuet-ai) becomes worth the spend; a fresh
non-Omarchy install fails to show SeaShells (would mean `theme.lua` fallback
broke); or CodeCompanion's `interactions`/adapter API changes across an update.
