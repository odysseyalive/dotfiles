;;; init.el --- Main Emacs Configuration -*- lexical-binding: t -*-

;; Author: Francis
;; Keywords: configuration

;;; Commentary:
;; YADRLite Emacs configuration - modular and modern.
;; Uses Vertico/Corfu/Consult for completion, Evil for Vim keybindings.
;; Leader key: , (comma)

;;; Code:

;; Add lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Add themes directory to custom theme load path
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))

;; Package management
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load configuration modules in order
;; Core settings (must come first)
(require 'init-core)

;; Evil mode (before keybindings)
(require 'init-evil)

;; UI and visual elements
(require 'init-ui)

;; Completion stack (Vertico/Corfu/Consult)
(require 'init-completion)

;; Custom functions (before keybindings that use them)
(require 'init-functions)

;; Keybindings (uses functions from above)
(require 'init-keybindings)

;; Tree-sitter for fast syntax highlighting (Emacs 29+)
(require 'init-treesitter)

;; Development tools
(require 'init-dev)

;; LSP and DAP
(require 'init-lsp)

;; Org-mode
(require 'init-org)

;; Git, Projectile, and other tools
(require 'init-tools)

;; Terminal configuration
(require 'init-terminal)

;; Email (Wanderlust)
(require 'init-mail)

;; GitHub Copilot - requires Node.js and Copilot subscription
;; Run M-x copilot-login after first load
(require 'init-copilot)

;; Grammarly - requires Node.js and Grammarly account
;; Run M-x lsp-grammarly-login after first load
(require 'init-grammarly)

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))
            (setq gc-cons-percentage 0.1)
            (run-with-idle-timer 2 t #'garbage-collect)))

;; Load Seashells dark theme
(load-theme 'seashells-dark t)

;; Display startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; init.el ends here
