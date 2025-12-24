;;; init-evil.el --- Evil Mode Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Evil mode and related plugins for Vim emulation.

;;; Code:

;; Pre-Evil settings
(setq evil-want-keybinding nil)
(setq evil-want-fine-undo t)
(setq evil-want-integration t)

;; Evil mode
(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-tree)
  :diminish evil-mode
  :config
  (evil-mode 1)
  ;; Paste from clipboard in normal mode
  (define-key evil-normal-state-map (kbd "C-v") 'clipboard-yank))

;; Evil matchit - jumping between matching tags
(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

;; Evil surround
(defun surround-drawer ()
  "Create org-mode drawer surround."
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format ":%s:" (upcase (or dname ""))) ":END:")))

(defun surround-code ()
  "Create org-mode code block surround."
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format "#+BEGIN_SRC %s" (or dname "")) "#+END_SRC")))

(use-package evil-surround
  :ensure t
  :diminish evil-surround-mode
  :config
  (add-to-list 'evil-surround-pairs-alist '(?: . surround-drawer))
  (add-to-list 'evil-surround-pairs-alist '(?# . surround-code))
  (global-evil-surround-mode 1))

;; Evil tutor
(use-package evil-tutor :ensure t)

;; Evil commentary - comment toggling
(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode 1))

;; Evil collection - bindings for many modes
(use-package evil-collection
  :ensure t
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

;; More Vim-like $ behavior
(evil-define-motion evil-last-non-blank (count)
  "Move the cursor to the last non-blank character on the current line.
If COUNT is given, move COUNT - 1 lines downward first."
  :type inclusive
  (evil-end-of-line count)
  (re-search-backward "^\\|[^[:space:]]")
  (setq evil-this-type (if (eolp) 'exclusive 'inclusive)))

(define-key evil-motion-state-map "g$" 'evil-end-of-line)
(define-key evil-motion-state-map "$" 'evil-last-non-blank)

;; Browse in emacs mode
(add-hook 'eww-mode-hook
          (lambda ()
            (evil-set-initial-state 'eww-mode 'emacs)))

(provide 'init-evil)
;;; init-evil.el ends here
