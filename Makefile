.PHONY: help test lint check install setup fmt

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

test: ## Run ShellSpec test suite
	@if command -v shellspec >/dev/null; then \
		shellspec --shell zsh; \
	elif [ -f ~/.local/lib/shellspec/shellspec ]; then \
		~/.local/lib/shellspec/shellspec --shell zsh; \
	else \
		echo "ShellSpec not found. Please install it: curl -fsSL https://git.io/shellspec | sh -s -- --yes"; \
		exit 1; \
	fi

lint: ## Run ShellCheck and basic Zsh syntax checks
	@echo "==> Linting POSIX install.sh and uninstall.sh with ShellCheck..."
	@if command -v shellcheck >/dev/null; then \
		shellcheck -s sh install.sh uninstall.sh; \
	else \
		echo "ShellCheck not found. Skipping lint for install.sh and uninstall.sh."; \
	fi
	@echo "==> Checking Zsh syntax for setup scripts..."
	@if command -v zsh >/dev/null; then \
		zsh -n setup.sh setup/*.sh setup/hooks/*/*.zsh setup/scripts/*.zsh setup/migrations/*/*.zsh; \
		echo "Zsh syntax OK."; \
	else \
		echo "Zsh not found. Skipping syntax check."; \
	fi

fmt: ## Format shell scripts using shfmt
	@if command -v shfmt >/dev/null; then \
		shfmt -l -w -i 2 -ci install.sh uninstall.sh setup.sh setup/*.sh setup/hooks/*/*.zsh setup/scripts/*.zsh setup/migrations/*/*.zsh; \
	else \
		echo "shfmt not found. Skipping format."; \
	fi

check: lint test ## Run all checks (linting and tests)

install: ## Run initial bootstrap (install.sh)
	./install.sh

setup: ## Show setup router help
	./setup.sh help
