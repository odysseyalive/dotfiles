# Contributing to YADRLite

First off, thank you for considering contributing to YADRLite!

## The Architecture
YADRLite uses a highly modular feature-router system based on **Zsh** and **Homebrew**. 

1. **`install.sh`**: Pure POSIX `sh` script. Installs Homebrew, Git, Zsh, authorizes Zsh in `/etc/shells`, changes the user's shell, and clones the repo.
2. **`setup.sh`**: The orchestrator. Parses OS flags (`--macos` or `--linux`) and feature flags (`--with-<feature>`).
3. **`brewfiles/<feature>.Brewfile`**: The Homebrew bundle listing dependencies for a feature.
4. **`setup/hooks/pre/<feature>.zsh`**: Commands that run *before* the Brewfile.
5. **`setup/hooks/post/<feature>.zsh`**: Commands that run *after* the Brewfile (e.g. symlinking, starting services).

## How to Add a New Feature

Want to add a new suite of tools (like Rust or Docker)? It's incredibly easy!

1. Create `brewfiles/docker.Brewfile` containing:
   ```ruby
   brew "docker"
   brew "docker-compose"
   ```
2. (Optional) Create `setup/hooks/post/docker.zsh` if you need to configure anything:
   ```zsh
   #!/usr/bin/env zsh
   setopt nullglob
   
   echo "==> Configuring Docker"
   # Setup logic here
   ```
3. Test your new feature locally:
   ```bash
   zsh ~/.yadrlite/setup.sh --with-docker
   ```

## Development Guidelines

1. **Format Shell Scripts**: Run `make fmt` (uses `shfmt`).
2. **Lint Shell Scripts**: Run `make lint` (uses `shellcheck` for `install.sh` and `zsh -n` for `setup/` hooks).
3. **Test Changes**: Run `make test` (uses ShellSpec).
4. **Use `setopt nullglob`**: Always place `setopt nullglob` at the top of your `.zsh` hooks to prevent the script from aborting when standard wildcards (e.g., `dir/*.txt`) don't match anything.

## Pull Requests
- Create a new branch off `master` (e.g., `feat/add-docker`).
- Write descriptive commit messages.
- Ensure `make check` passes locally.
- Our GitHub Actions CI will automatically test, lint, and generate a terminal demo GIF of your changes using VHS!
