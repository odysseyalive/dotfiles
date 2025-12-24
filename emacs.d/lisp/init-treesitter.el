;;; init-treesitter.el --- Tree-sitter Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Tree-sitter for fast, accurate syntax highlighting.
;; Requires Emacs 29+ with tree-sitter support.
;;
;; Grammars are auto-installed on first use.

;;; Code:

;; Check for tree-sitter support
(when (and (fboundp 'treesit-available-p)
           (treesit-available-p))

  ;; Where to install tree-sitter grammars
  (setq treesit-extra-load-path
        (list (expand-file-name "tree-sitter" user-emacs-directory)))

  ;; Auto-install grammars when needed
  (use-package treesit-auto
    :ensure t
    :demand t
    :config
    (setq treesit-auto-install 'prompt)  ; Ask before installing

    ;; Languages to set up (add more as needed)
    (setq treesit-auto-langs
          '(bash
            c
            cmake
            commonlisp
            cpp
            css
            dockerfile
            go
            gomod
            html
            java
            javascript
            json
            lua
            make
            markdown
            php
            python
            ruby
            rust
            toml
            tsx
            typescript
            yaml))

    ;; Enable global mode to auto-remap to ts modes
    (global-treesit-auto-mode))

  ;; Font-lock settings for tree-sitter
  ;; Level 4 = maximum detail (all syntax elements highlighted)
  (setq treesit-font-lock-level 4)

  ;; Increase chunk size for better performance on large files
  (setq treesit-max-buffer-size (* 1024 1024 10))  ; 10MB

  ;; Indentation via tree-sitter
  (setq treesit-simple-indent-rules nil))

;; Fallback message if tree-sitter not available
(unless (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  (message "Tree-sitter not available. Using traditional font-lock."))

(provide 'init-treesitter)
;;; init-treesitter.el ends here
