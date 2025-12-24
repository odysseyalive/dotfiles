;;; init-copilot.el --- GitHub Copilot Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; GitHub Copilot integration for Emacs.
;; Requires: Node.js 18+, GitHub Copilot subscription
;;
;; First-time setup:
;;   M-x copilot-login
;;
;; Usage:
;;   Completions appear automatically as you type.
;;   Tab to accept, M-] for next suggestion, M-[ for previous.

;;; Code:

;; quelpa for installing packages from GitHub
(use-package quelpa
  :ensure t
  :config
  (setq quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :ensure t
  :after quelpa)

;; Inline completions
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el"))
  :defer t  ; Don't load until manually enabled or server is installed
  :commands (copilot-mode copilot-login)
  :init
  ;; Only auto-enable if copilot server is installed
  (defun yadrlite--maybe-enable-copilot ()
    "Enable copilot-mode only if the server is installed."
    (when (and (fboundp 'copilot-mode)
               (ignore-errors (copilot-server-executable)))
      (copilot-mode 1)))
  :hook ((prog-mode . yadrlite--maybe-enable-copilot)
         (text-mode . yadrlite--maybe-enable-copilot))
  :bind (:map copilot-completion-map
              ("TAB" . copilot-accept-completion)
              ("<tab>" . copilot-accept-completion)
              ("M-]" . copilot-next-completion)
              ("M-[" . copilot-previous-completion)
              ("M-<right>" . copilot-accept-completion-by-word)
              ("M-RET" . copilot-accept-completion-by-line)
              ("C-g" . copilot-clear-overlay))
  :config
  ;; Indent with completion for these modes
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
  (add-to-list 'copilot-indentation-alist '(php-mode 4))

  ;; Don't show completions when:
  (setq copilot-disable-predicates
        '(;; Buffer is read-only
          (lambda () buffer-read-only)
          ;; In minibuffer
          (lambda () (minibufferp))
          ;; In company/corfu completion
          (lambda () (and (bound-and-true-p corfu--frame)
                          (frame-visible-p corfu--frame))))))

;; Copilot Chat
(use-package copilot-chat
  :quelpa (copilot-chat :fetcher github
                        :repo "chep/copilot-chat.el"
                        :branch "master"
                        :files ("*.el"))
  :after copilot
  :bind (("C-c C-c" . copilot-chat-explain)
         ("C-c C-r" . copilot-chat-review)
         ("C-c C-f" . copilot-chat-fix)
         ("C-c C-o" . copilot-chat-optimize))
  :config
  ;; Use markdown for chat display
  (setq copilot-chat-frontend 'markdown))

;; Keybindings with leader key
(with-eval-after-load 'general
  (general-define-key
   :prefix ","
   :non-normal-prefix "M-,"
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "c"  '(nil :which-key "Copilot")
   "cc" '(copilot-chat-display :which-key "Open Chat")
   "ce" '(copilot-chat-explain :which-key "Explain Code")
   "cf" '(copilot-chat-fix :which-key "Fix Code")
   "co" '(copilot-chat-optimize :which-key "Optimize Code")
   "cr" '(copilot-chat-review :which-key "Review Code")
   "ct" '(copilot-chat-test :which-key "Generate Tests")
   "cd" '(copilot-chat-doc :which-key "Generate Docs")
   "cl" '(copilot-login :which-key "Login to Copilot")
   "cs" '(copilot-mode :which-key "Toggle Copilot")))

(provide 'init-copilot)
;;; init-copilot.el ends here
