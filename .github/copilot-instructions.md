# Copilot Code Review Instructions

When reviewing pull requests in this repository (YADRLite), please observe the following strictly enforced architectural rules:

1. **Two-Stage Architecture**: YADRLite separates the initial bootstrap (`install.sh`) from the post-installation management (`setup.sh`). Do not suggest adding post-installation logic (e.g., `git clone` or downloading tools) to `install.sh`. 
2. **POSIX Compliance**: `install.sh` MUST be written in pure POSIX `sh` (`#!/bin/sh`). Do not suggest Bashisms (`[[ ]]`, `source`, arrays) in `install.sh`.
3. **Zsh Enforced**: `setup.sh` and everything inside `setup/hooks/` MUST be written in Zsh (`#!/usr/bin/env zsh`). `install.sh` guarantees that Homebrew and Zsh are installed, so you can safely rely on Zsh features.
4. **Zsh Globbing**: Suggest adding `setopt nullglob` to the top of any new `.zsh` script that uses wildcards (e.g., `*.ttf`) to prevent `nomatch` errors from aborting the script if a directory is empty.
5. **Homebrew Enforced**: Reject suggestions to use `apt`, `yum`, `pacman`, or manual `curl | tar` commands for standard binaries. Suggest adding them to a `.Brewfile` instead.
6. **No OS Detection for Binaries**: Do not suggest `uname -m` or `uname -s` checks to download platform-specific binaries. Rely on `brew install` to handle the architecture and OS automatically.
7. **Modularity**: Suggest extracting large blocks of logic from `setup.sh` into feature-specific hooks (`setup/hooks/pre/<feature>.zsh` and `setup/hooks/post/<feature>.zsh`) and `brewfiles/<feature>.Brewfile` using the `--with-<feature>` CLI pattern.
