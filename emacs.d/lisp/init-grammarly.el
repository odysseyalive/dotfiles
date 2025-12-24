;;; init-grammarly.el --- Grammarly Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Grammarly integration via LSP.
;; Requires: Node.js, Grammarly account (free tier works)
;;
;; First-time setup:
;;   M-x lsp-grammarly-login
;;
;; Activates automatically in text-mode, org-mode, markdown-mode.

;;; Code:

(use-package lsp-grammarly
  :ensure t
  :hook ((text-mode . lsp-deferred)
         (org-mode . lsp-deferred)
         (markdown-mode . lsp-deferred))
  :config
  ;; Use Grammarly for these modes
  (setq lsp-grammarly-auto-activate t)

  ;; Grammarly suggestions appear as diagnostics
  ;; Adjust severity if needed (1=error, 2=warning, 3=info, 4=hint)
  (setq lsp-grammarly-suggestion-severity 3)

  ;; Domain affects writing style suggestions
  ;; Options: academic, business, general, technical, casual, creative
  (setq lsp-grammarly-domain "general")

  ;; Audience affects formality suggestions
  ;; Options: knowledgeable, expert, general
  (setq lsp-grammarly-audience "general"))

;; Keybindings with leader key
(with-eval-after-load 'general
  (general-define-key
   :prefix ","
   :non-normal-prefix "M-,"
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "gl" '(lsp-grammarly-login :which-key "Login to Grammarly")
   "gg" '(lsp-grammarly-check-grammar :which-key "Check Grammar")))

(provide 'init-grammarly)
;;; init-grammarly.el ends here
