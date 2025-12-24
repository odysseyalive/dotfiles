;;; init-tools.el --- Tools Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Git, projectile, file management, and other tools.

;;; Code:

;; Projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :demand t  ; Load immediately, don't defer
  :custom
  (projectile-completion-system 'default)
  (projectile-enable-caching t)
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Perspective - per-project workspaces
(use-package perspective
  :ensure t
  :demand t
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  (persp-state-default-file (expand-file-name "persp-state" user-emacs-directory))
  :init
  (persp-mode)
  :config
  ;; Auto-save perspectives on quit and switch
  (add-hook 'kill-emacs-hook #'persp-state-save)
  (add-hook 'persp-switch-hook #'persp-state-save)
  ;; Load saved perspectives on startup
  (when (file-exists-p persp-state-default-file)
    (persp-state-load persp-state-default-file)))

;; Bridge perspective with projectile
(use-package persp-projectile
  :ensure t
  :after (perspective projectile))

;; Magit
(use-package magit
  :ensure t
  :diminish magit-mode
  :commands magit-status)

;; Git gutter
(use-package fringe-helper :ensure t)

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode +1))

(use-package git-gutter-fringe
  :ensure t
  :if (display-graphic-p))

;; Neotree
(use-package neotree
  :ensure t
  :custom
  (neo-cwd-line-style 'text)
  (neo-fit-to-contents t)
  (neo-show-hidden-files t)
  (neo-smart-open t)
  (neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  (neo-vc-integration '(face char))  ; Show both color and character for git status
  (neo-window-position 'left)
  (neo-window-width 50)
  (neo-autorefresh t)  ; Auto-refresh on file changes
  :config
  ;; Use projectile to find project root
  (setq neo-force-change-root t)
  (add-hook 'neotree-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter-hide)
              (define-key evil-normal-state-local-map (kbd "gr") 'neotree-refresh))))

(defun neo-open-file-hide (full-path &optional arg)
  "Open a file node and hides tree."
  (neo-global--select-mru-window arg)
  (find-file full-path)
  (neotree-hide))

(defun neotree-enter-hide (&optional arg)
  "Enters file and hides neotree directly."
  (interactive "P")
  (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))

;; Ranger
(use-package ranger :ensure t)

;; Tramp
(use-package tramp :ensure t)

;; Spell Check
(use-package auto-dictionary
  :ensure t
  :hook (flyspell-mode . auto-dictionary-mode))

(use-package flyspell-correct
  :ensure t
  :after flyspell)

;; Dictionary / Thesaurus
(use-package wordnut
  :ensure t
  :config
  (add-hook 'wordnut-mode-hook
            (lambda ()
              (define-key evil-normal-state-local-map (kbd "p") 'wordnut-history-backward)
              (define-key evil-normal-state-local-map (kbd "n") 'wordnut-history-forward)
              (define-key evil-normal-state-local-map (kbd "?") 'wordnut-history-lookup))))

;; Powerthesaurus
(use-package powerthesaurus :ensure t)

;; w3m browser
(use-package w3m :ensure t)
(setq browse-url-browser-function 'w3m-browse-url)

;; Scratch buffer
(use-package scratch
  :ensure t
  :diminish scratch-mode)

(provide 'init-tools)
;;; init-tools.el ends here
