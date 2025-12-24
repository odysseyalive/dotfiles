;;; init.el --- Main Emacs Configuration -*- lexical-binding: t -*-

;; Author: Francis
;; Keywords: configuration

;;; Commentary:
;; YADRLite Emacs configuration - modular and modern.
;; Uses Vertico/Corfu/Consult for completion, Evil for Vim keybindings.
;; Leader key: , (comma)

;;; Code:

;; Show startup progress
(defvar yadrlite--init-start-time (current-time))
(defvar yadrlite--first-run (not (file-exists-p (expand-file-name "elpa" user-emacs-directory))))

(defun yadrlite--display-startup-message (msg)
  "Display MSG in a centered buffer during startup."
  (when yadrlite--first-run
    (with-current-buffer (get-buffer-create "*Startup*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n\n\n")
        (insert (propertize "  YADRLite Emacs\n" 'face '(:height 1.5 :weight bold)))
        (insert "\n")
        (insert (format "  %s\n" msg))
        (insert "\n  This may take a few minutes on first launch...\n"))
      (goto-char (point-min))
      (special-mode))
    (switch-to-buffer "*Startup*")
    (redisplay t)))

(when yadrlite--first-run
  (yadrlite--display-startup-message "Installing packages..."))

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

;; Display startup time and cleanup
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Kill startup buffer if it exists
            (when (get-buffer "*Startup*")
              (kill-buffer "*Startup*"))
            ;; Show startup message
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; init.el ends here
