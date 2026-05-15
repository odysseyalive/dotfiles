# YADRLite Setup Reference

## Initial Installation

Remote bootstrap:
```bash
bash -c "`curl -fsSL https://raw.githubusercontent.com/odysseyalive/dotfiles/master/install.sh`"
```

The `install.sh` script:
- Clones the repository to `~/.yadrlite` (if run remotely)
- Backs up existing dotfiles to `~/.yadrlite/backup/`
- Creates symbolic links in `$HOME` for base config files
- Clones and sets up core tmux plugins

## Install Development Tools

```bash
zsh ~/.yadrlite/setup.zsh tools
```

Installs:
- TypeScript/JavaScript: eslint, babel-eslint, js-beautify
- PHP: intelephense
- Go utilities: fzf, lazygit, gitleaks, glow

## Language Management (ASDF)

```bash
zsh ~/.yadrlite/setup.zsh --with-langs                # all defaults
zsh ~/.yadrlite/setup.zsh --with-lang-ruby-3.2.0      # single language + version
```

Manages Node.js, Python, Ruby, and Golang installations globally via the ASDF version manager.

## Apply Rolling Migrations

```bash
zsh ~/.yadrlite/setup.zsh --migrate
```

Executes cumulative pre/post hooks found in `setup/migrations/v*` (e.g., migrating from legacy NVM to ASDF) automatically.

## Update Dotfiles

```bash
zsh ~/.yadrlite/setup.zsh update
```

## Uninstall and Restore

```bash
zsh ~/.yadrlite/setup.zsh remove
```

Restores backups from `~/.yadrlite/backup/` and uninstalls YADRLite.

## macOS Workstation

```bash
zsh ~/.yadrlite/setup.zsh macos       # AeroSpace, Sketchybar, Ghostty
zsh ~/.yadrlite/setup.zsh keyboard    # swap Caps Lock and Escape (opt-in)
```

## Arch Linux Workstation Setup

Omarchy:
```bash
curl https://raw.githubusercontent.com/odysseyalive/dotfiles/master/workstation/omarchy-latest > ~/install
bash install
```

EndeavourOS:
```bash
curl https://raw.githubusercontent.com/odysseyalive/dotfiles/master/workstation/endeavouros-mercury-neo > ~/install
bash install
```

## Backup Strategy

All original dotfiles are backed up to `~/.yadrlite/backup/` before installation. The `remove` action restores from this location.

## Shell Integration

The setup script intelligently merges bash configurations by detecting whether `~/.bashrc` is sourced from `~/.bash_profile`, and detects the user's shell to configure tmux accordingly.
