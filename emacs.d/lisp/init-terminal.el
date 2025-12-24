;;; init-terminal.el --- Terminal Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Terminal and shell configuration.

;;; Code:

;; Multi-term
(setq multi-term-program "/bin/bash")

(use-package multi-term
  :ensure t
  :config
  (add-hook 'term-mode-hook
            (lambda ()
              (evil-set-initial-state 'term-mode 'emacs)
              (setq term-buffer-maximum-size 25000)
              (setq show-trailing-whitespace nil)
              (prefer-coding-system 'utf-8)
              (define-key term-raw-map (kbd "C-v") 'term-paste)
              (define-key term-raw-map (kbd "<prior>") 'scroll-down)
              (define-key term-raw-map (kbd "<next>") 'scroll-up)
              (define-key term-raw-map (kbd "<escape>") 'term-send-esc)
              (define-key term-raw-map (kbd "<delete>") 'term-send-backspace)
              (define-key term-raw-map (kbd "<backspace>") 'term-send-backspace))))

;; Toggle line mode when flipping states
(add-hook 'evil-normal-state-entry-hook
          (lambda ()
            (when (eq major-mode 'term-mode)
              (term-line-mode))))

(add-hook 'evil-emacs-state-entry-hook
          (lambda ()
            (when (eq major-mode 'term-mode)
              (when (term-in-line-mode)
                (condition-case nil
                    (term-char-mode)
                  (error nil))))))

(defun term-toggle-mode ()
  "Toggles term between line mode and char mode."
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

;; Exec path from shell (for macOS mainly)
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(provide 'init-terminal)
;;; init-terminal.el ends here
