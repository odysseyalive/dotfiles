# YADRLite Testing Reference

## Tooling

| Tool | Purpose |
|------|---------|
| **ShellSpec** | BDD testing framework for `install.sh` and the `setup/*` scripts. See https://shellspec.info/ |
| **ShellCheck** | Static analysis on `install.sh` for POSIX `sh` compliance and safety |
| **Zsh Syntax** | `make lint` runs `zsh -n` across all `.sh` files in `setup/` to catch syntax errors before execution |

## Makefile Targets

Run `make help` to list all targets. Key entry points:

- `make check` — full test suite + linter
- `make lint` — shell syntax checks only
- `make test` — ShellSpec tests only

## CI

`.github/workflows/ci.yml` runs the `make check` suite on both Ubuntu and macOS to ensure cross-platform compliance.

## Conventions

- Tests live alongside the scripts they cover, following ShellSpec's discovery pattern
- `install.sh` is expected to remain POSIX-compliant (validated by ShellCheck)
- `setup/*.sh` may use zsh features, validated by `zsh -n` syntax check
