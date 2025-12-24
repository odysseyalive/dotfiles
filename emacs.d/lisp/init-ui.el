;;; init-ui.el --- UI Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Theme, modeline, icons, fonts configuration.

;;; Code:

;; Theme toggle function
(defvar seashells-current-theme 'dark
  "Current seashells theme variant: 'dark or 'light")

(defun toggle-light-dark-theme ()
  "Toggle between Seashells dark and light themes."
  (interactive)
  (if (eq seashells-current-theme 'dark)
      (progn
        (disable-theme 'seashells-dark)
        (load-theme 'seashells-light t)
        (setq seashells-current-theme 'light)
        (message "Switched to Seashells Light theme"))
    (progn
      (disable-theme 'seashells-light)
      (load-theme 'seashells-dark t)
      (setq seashells-current-theme 'dark)
      (message "Switched to Seashells Dark theme"))))

;; Doom modeline (replaces spaceline)
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 3)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-lsp-icon nil)
  (doom-modeline-modal-icon t))

;; Nerd icons (replaces all-the-icons)
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Avy (replaces ace-jump-mode)
(use-package avy
  :ensure t
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'at-full)
  (avy-background t))

;; Which-key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode 1))

;; Fira code ligatures
(use-package fira-code-mode
  :ensure t
  :if (display-graphic-p)
  :custom
  (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :hook prog-mode
  :config
  (fira-code-mode-set-font))

;; Font configuration
(when (display-graphic-p)
  (set-face-attribute 'default nil
                      :font "FiraCode Nerd Font Mono"
                      :height 120))

;; Emojify
(use-package emojify
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . global-emojify-mode))

;; Fancy battery
(use-package fancy-battery
  :ensure t
  :config
  (fancy-battery-mode 1))

;; Dashboard
(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode)

(use-package dashboard
  :ensure t
  :custom
  (dashboard-startup-banner 'official)
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-init-info t)
  (dashboard-week-agenda t)
  (dashboard-banner-logo-title "Welcome to YADRLite")
  (dashboard-items '((recents . 10)
                     (bookmarks . 5)
                     (projects . 10)))
  :config
  (dashboard-modify-heading-icons '((bookmarks . "book")))
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(defun call-dashboard ()
  "Jump to the dashboard buffer, if doesn't exist create one."
  (interactive)
  (dashboard-refresh-buffer)
  (switch-to-buffer dashboard-buffer-name)
  (dashboard-mode)
  (dashboard-insert-startupify-lists))

;; Evil cursor for terminal
(unless (display-graphic-p)
  (use-package evil-terminal-cursor-changer
    :ensure t
    :config
    (evil-terminal-cursor-changer-activate)))

(provide 'init-ui)
;;; init-ui.el ends here
