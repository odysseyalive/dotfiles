;;; init-org.el --- Org Mode Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Org-mode settings for task management, notes, and exports.

;;; Code:

;; Set Clock Format
(setq org-time-clocksum-format
      (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))

;; Symmetric Inline Encryption
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)
(setq auto-save-default nil)

;; Symmetric File Encryption
(require 'epa-file)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
(epa-file-enable)

;; Startup folded
(setq org-startup-folded t)

;; Hide emphasis marks
(setq org-hide-emphasis-markers t)

;; Clock drawer
(setq org-clock-into-drawer t)

;; Org bullets for visual hierarchy
(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config
  (add-hook 'org-mode-hook
            (lambda ()
              (toggle-truncate-lines)
              (local-set-key (kbd "C-v") 'clipboard-yank))))

;; Org mode Evil compatibility workaround for terminal
(add-hook 'org-mode-hook
          (lambda ()
            (define-key evil-normal-state-map (kbd "C-h") 'org-metaleft)
            (define-key evil-normal-state-map (kbd "C-k") 'org-metaup)
            (define-key evil-normal-state-map (kbd "C-j") 'org-metadown)
            (define-key evil-normal-state-map (kbd "C-l") 'org-metaright)
            (define-key evil-normal-state-map "H" 'org-shiftleft)
            (define-key evil-normal-state-map "K" 'org-shiftup)
            (define-key evil-normal-state-map "J" 'org-shiftdown)
            (define-key evil-normal-state-map "L" 'org-shiftright)
            (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)))

;; Export to epub
(use-package ox-epub :ensure t)

;; Better agenda view with org-ql
(use-package org-ql :ensure t)

(defun better-agenda-view ()
  "View All Agenda todo items plus past and current deadlines."
  (interactive)
  (org-ql-search (org-agenda-files)
    '(and (todo "EMERGENCY" "TODAY" "TODO" "WAITING"))))

;; Artist Mode (for drawing)
(evil-set-initial-state 'artist-mode 'emacs)

(defun toggle-artist-mode ()
  "Toggles artist mode."
  (interactive)
  (if (bound-and-true-p artist-mode)
      (artist-mode-off)
    (artist-mode)))

;; Fix page numbers in table of contents for Orgmode exports
(setq org-latex-pdf-process
      '("lualatex -shell-escape -interaction nonstopmode %f"
        "lualatex -shell-escape -interaction nonstopmode %f"))

;; Plantuml support
(use-package plantuml-mode :ensure t)
(use-package flycheck-plantuml :ensure t)
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
(setq org-plantuml-jar-path
      (expand-file-name "~/.yadrlite/src/org/contrib/scripts/plantuml.jar"))

(provide 'init-org)
;;; init-org.el ends here
