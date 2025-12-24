;;; init-core.el --- Core Settings -*- lexical-binding: t -*-

;;; Commentary:
;; Core Emacs settings: backups, encoding, line numbers, etc.

;;; Code:

;; Handles Backup Files
(setq backup-by-copying t
      backup-directory-alist '((".*" . "~/.emacs.d/.emacs-saves"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/.emacs-saves" t))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      undo-limit (* 1024 1024 1024))

;; Remember your place
(save-place-mode 1)

;; Tabs to spaces
(setq-default indent-tabs-mode nil)
;; Tab width
(setq-default tab-width 4)
(setq c-basic-offset 4)
(setq indent-line-function 'insert-tab)

;; Turn off line wrapping by default
(set-default 'truncate-lines t)

;; Turn off alarms
(setq ring-bell-function 'ignore)

;; Set the default encoding system
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; GPUPG Fix
(setf epa-pinentry-mode 'loopback)

;; Suppress warnings
(setq warning-minimum-level :emergency)

;; Show recently opened files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)

;; Clipboard sharing
(setq x-select-enable-clipboard t)
(setq x-select-enable-primary t)

;; Smooth scrolling
(pixel-scroll-mode 1)
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq pixel-scroll-precision-interpolation-factor 30)

;; Helps prevent excess cpu during visual mode
(setq auto-window-vscroll nil)

;; Allow font resizing
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Line numbers in prog-mode
(defun my-display-numbers-hook ()
  "Display line numbers in programming modes."
  (display-line-numbers-mode t))
(add-hook 'prog-mode-hook 'my-display-numbers-hook)

;; Show matching parens
(show-paren-mode 1)

;; Electric pair mode (replaces phi-autopair)
(electric-pair-mode 1)

;; For Diminishing TMI stuff on the mode line
(use-package diminish :ensure t)

;; Persistent undo history
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

(provide 'init-core)
;;; init-core.el ends here
