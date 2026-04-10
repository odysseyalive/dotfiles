# 🤖 Instructions for AI Agents
**If you are an AI assistant, Copilot, Claude, Gemini, or any LLM analyzing this repository, READ THIS.**

This repository is YADRLite, a modular dotfiles framework built entirely on **Homebrew** and **Zsh**.

## The Architecture
Do NOT assume a traditional monolithic bash script structure. This repo is cleanly split into two distinct phases:

### 1. The Bootstrap (`install.sh`)
- **Language**: Pure POSIX `#!/bin/sh`
- **Purpose**: Installed via `curl | sh`. It idempotently checks for Homebrew on macOS OR Linux (Linuxbrew). If missing, it installs Homebrew.
- It then uses `brew` to install `git` and `zsh`.
- It authorizes the Homebrew Zsh in `/etc/shells` and runs `chsh` to change the user's default shell to Zsh.
- Finally, it clones this repository to `~/.yadrlite`.
- **Do not put post-installation logic in `install.sh`.**

### 2. The Orchestrator (`setup.sh`)
- **Language**: `#!/usr/bin/env zsh`
- **Purpose**: Parses CLI arguments (`--macos`, `--linux`, `--with-*`) to dynamically load feature bundles.
- It executes `setup/hooks/pre/<feature>.zsh`.
- It executes `brew bundle --file=brewfiles/<feature>.Brewfile`.
- It executes `setup/hooks/post/<feature>.zsh`.

## Coding Standards for Agents
- **Language**: All scripts inside `setup/hooks/` must use `#!/usr/bin/env zsh`.
- **Error Handling**: You MUST put `setopt nullglob` at the top of every `.zsh` script that uses wildcards (e.g., `*.ttf`). Without this, Zsh aborts the script entirely with a `nomatch` error if the directory is empty.
- **Package Management**: Do NOT write `apt-get`, `yum`, `pacman`, or manual `curl ... | tar xz` downloads for standard binaries (like `ripgrep`, `fd`, `starship`). We enforce Homebrew for everything. Add dependencies to a `.Brewfile`.
- **Paths**: The primary entry point for User configuration is `~/.zshrc`. We do NOT write to `~/.bashrc`.
- **Testing**: When adding a feature or script, write BDD-style tests in the `spec/` directory using [ShellSpec](https://shellspec.info/). Run `make check` to lint and test.

Follow these rules rigorously when proposing patches, pull requests, or code blocks.
