;;; init-dev.el --- Development Mode Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Language modes, syntax checking, and development tools.

;;; Code:

;; Flycheck - Syntax checking (LSP auto-enables per buffer)
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  ;; Don't use global-flycheck-mode - LSP enables it per-buffer automatically
  :custom
  (flycheck-checker-error-threshold nil)
  :config
  (flycheck-add-mode 'html-tidy 'web-mode)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos)
                 (display-buffer-reuse-window
                  display-buffer-in-side-window)
                 (side . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.15))))

(use-package flycheck-color-mode-line
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(defun flycheck-list-errors-toggle ()
  "Toggle the error list for the current buffer."
  (interactive)
  (let ((flycheck-errors-window (get-buffer-window flycheck-error-list-buffer)))
    (if (not (window-live-p flycheck-errors-window))
        (call-interactively 'flycheck-list-errors)
      (delete-window flycheck-errors-window))))

;; YASnippet
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; Aggressive indentation
(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode)

;; Web mode
(use-package web-mode
  :ensure t
  :diminish web-mode
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.htm[l]?\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.js\\'" . web-mode))
  :hook (web-mode . (lambda ()
                      (local-set-key '[backtab] 'indent-relative)
                      (rainbow-mode 1)
                      (setq indent-tabs-mode nil
                            web-mode-enable-auto-pairing t
                            web-mode-enable-comment-interpolation t
                            web-mode-enable-heredoc-fontification t
                            web-mode-enable-current-element-highlight t
                            web-mode-enable-current-column-highlight t
                            web-mode-markup-indent-offset 2
                            web-mode-css-indent-offset 2
                            web-mode-code-indent-offset 2))))

;; PHP Mode
(use-package php-mode
  :ensure t
  :diminish php-mode
  :mode (("\\.php\\'" . php-mode)
         ("\\.tpl\\'" . php-mode)
         ("\\.inc\\'" . php-mode))
  :hook (php-mode . (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4
                            c-basic-offset 4
                            php-mode-coding-style 'psr2)))
  :config
  ;; Disable all PHP flycheck checkers globally - use LSP instead
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(php php-phpcs php-phpmd))))
  ;; Format on save using LSP
  (add-hook 'php-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

(defun toggle-php-flavor-mode ()
  "Toggle mode between PHP & Web-Mode Helper modes."
  (interactive)
  (cond ((string= mode-name "PHP/l")
         (web-mode))
        ((string= mode-name "Web")
         (php-mode))))

;; TypeScript
(use-package typescript-mode
  :ensure t
  :diminish typescript-mode
  :mode "\\.ts\\'")

;; Sass/SCSS
(use-package sass-mode
  :ensure t
  :diminish sass-mode)

;; JSON
(use-package json-mode
  :ensure t
  :diminish json-mode
  :mode "\\.json\\'")

;; TOML
(use-package toml-mode
  :ensure t
  :diminish toml-mode
  :mode "\\.toml\\'")

;; Markdown
(use-package markdown-mode
  :ensure t
  :diminish markdown-mode
  :mode "\\.md\\'")

;; YAML
(use-package yaml-mode
  :ensure t
  :diminish yaml-mode
  :mode "\\.yml\\'")

;; CoffeeScript
(use-package coffee-mode
  :ensure t
  :diminish coffee-mode
  :mode "\\.coffee\\'")

;; Go
(use-package go-mode
  :ensure t
  :diminish go-mode
  :mode "\\.go\\'")

;; CSS mode
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; Rainbow mode for color codes
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode)

;; Web Beautify
(use-package web-beautify :ensure t)

(defun beautify-code ()
  "Beautify CSS, HTML or JS."
  (interactive)
  (cond
   ((eq major-mode 'js-mode) (web-beautify-js))
   ((eq major-mode 'json-mode) (web-beautify-js))
   ((eq major-mode 'sgml-mode) (web-beautify-html))
   ((eq major-mode 'web-mode) (web-beautify-html))
   ((eq major-mode 'xah-css-mode) (web-beautify-css))
   ((eq major-mode 'css-mode) (web-beautify-css))
   ((eq major-mode 'scss-mode) (web-beautify-css))
   (t (indent-region (point-min) (point-max)))))

(provide 'init-dev)
;;; init-dev.el ends here
