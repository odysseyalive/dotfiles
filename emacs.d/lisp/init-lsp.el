;;; init-lsp.el --- LSP Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Language Server Protocol and Debug Adapter Protocol configuration.

;;; Code:

;; Intelephense license key
(setq lsp-intelephense-licence-key
      (expand-file-name "~/.config/intelephense/licence.txt"))

;; LSP Mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((php-mode . lsp-deferred)
         (sass-mode . lsp-deferred)
         (css-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (toml-mode . lsp-deferred)
         (json-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-idle-delay 0.500)
  (lsp-log-io nil)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold nil)
  (lsp-modeline-code-actions-enable t)
  (lsp-modeline-diagnostics-enable t)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-warn-no-matched-clients nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-completion-provider :none)  ; Use Corfu instead
  (lsp-headerline-breadcrumb-enable t)
  (lsp-semantic-tokens-enable t)
  :config
  ;; Performance optimizations
  (setq read-process-output-max (* 1024 1024))
  ;; Clear workspace folders on restart
  (advice-add 'lsp :before
              (lambda (&rest _args)
                (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  ;; Register Corfu as completion provider
  (setq lsp-completion-provider :none)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  (add-hook 'lsp-completion-mode-hook #'my/lsp-mode-setup-completion))

;; LSP UI
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-enable t)
  (lsp-ui-peek-list-width 60)
  (lsp-ui-peek-peek-height 25)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)))

;; LSP doc toggle function
(defvar lsp-ui-doc-visible nil
  "Track LSP UI doc visibility.")

(defun lsp-ui-doc-toggle ()
  "Toggle LSP UI documentation."
  (interactive)
  (if lsp-ui-doc-visible
      (progn
        (lsp-ui-doc-hide)
        (setq lsp-ui-doc-visible nil))
    (progn
      (lsp-ui-doc-show)
      (setq lsp-ui-doc-visible t))))

;; LSP Treemacs
(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :commands lsp-treemacs-errors-list
  :config
  (setq lsp-treemacs-sync-mode 1)
  ;; Fix for Emacs 27+
  (defun lsp-treemacs--generic-icon (item expanded?)
    "Get the symbol for the the kind."
    (concat
     (if (or (plist-get item :children)
             (plist-get item :children-async))
         (if expanded? "▾ " "▸ ")
       "  ")
     (or (plist-get item :icon-literal)
         (if-let (icon (plist-get item :icon))
             (treemacs-get-icon-value
              icon
              nil
              lsp-treemacs-theme)
           "   ")))))

;; DAP Mode - Debug Adapter Protocol
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode)
         (python-mode . dap-mode))
  :diminish dap-mode
  :custom
  (dap-auto-configure-features '(locals controls tooltip))
  (dap-ui-variable-length 10000)
  :config
  (require 'dap-php)
  (require 'dap-python)
  (require 'dap-lldb)
  (defun dap-python--pyenv-executable-find (command)
    (with-venv (executable-find "python"))))

;; Complete DAP termination
(defun terminate-dap-mode ()
  "Terminate DAP mode completely."
  (interactive)
  (dap-delete-all-sessions)
  (dap-mode 0)
  (kill-matching-buffers ".*launch\.j.*" t t))

;; Grammarly LSP (optional)
(use-package lsp-grammarly
  :ensure t
  :init
  (setq lsp-grammarly-active-modes
        '(mu4e-compose-mode text-mode latex-mode org-mode markdown-mode))
  :hook (org-mode . (lambda ()
                      (require 'lsp-grammarly)
                      (lsp-deferred))))

(provide 'init-lsp)
;;; init-lsp.el ends here
