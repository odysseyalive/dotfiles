;;; seashells-light-theme.el --- Seashells Light Theme -*- lexical-binding: t -*-

;; Author: Francis
;; Version: 1.0
;; Keywords: faces, theme
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:
;; A light color theme based on the Seashells palette.

;;; Code:

(deftheme seashells-light "Seashells light color theme")

(let ((class '((class color) (min-colors 89)))
      ;; Primary palette
      (bg-main      "#e0d6c8")
      (bg-alt       "#d4c9b8")
      (bg-dim       "#c8bda8")
      (fg-main      "#0f2838")
      (fg-dim       "#4a5a65")
      (fg-faint     "#6a7a85")

      ;; Accent colors
      (accent       "#50a3b5")
      (accent-dark  "#027b9b")
      (accent-dim   "#0e8fb5")
      (teal         "#2d6870")
      (teal-light   "#3a6a75")
      (cyan         "#027b9b")

      ;; Semantic colors
      (red          "#d05023")
      (red-light    "#d38677")
      (orange       "#d88821")
      (orange-light "#c57a1a")
      (yellow       "#d88821")
      (green        "#2d6870")
      (blue         "#50a3b5")
      (magenta      "#2d6870")
      (violet       "#0f7b8a")

      ;; UI colors
      (selection    "#c8dde8")
      (highlight    "#d4c9b8")
      (cursor       "#d05023")
      (border       "#b8a796")
      (border-active "#50a3b5")

      ;; Diff colors
      (diff-add-bg  "#c8e8d8")
      (diff-del-bg  "#e8c8c8")
      (diff-chg-bg  "#e8e0c8"))

  (custom-theme-set-faces
   'seashells-light

   ;; Basic faces
   `(default ((,class (:background ,bg-main :foreground ,fg-main))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,selection :extend t))))
   `(highlight ((,class (:background ,highlight))))
   `(hl-line ((,class (:background ,bg-alt :extend t))))
   `(fringe ((,class (:background ,bg-main :foreground ,fg-dim))))
   `(vertical-border ((,class (:foreground ,border))))
   `(border ((,class (:foreground ,border))))
   `(shadow ((,class (:foreground ,fg-faint))))
   `(minibuffer-prompt ((,class (:foreground ,accent :weight bold))))
   `(link ((,class (:foreground ,accent :underline t))))
   `(link-visited ((,class (:foreground ,magenta :underline t))))
   `(escape-glyph ((,class (:foreground ,orange))))
   `(homoglyph ((,class (:foreground ,orange))))
   `(match ((,class (:background ,selection :foreground ,fg-main))))
   `(isearch ((,class (:background ,orange :foreground ,bg-main :weight bold))))
   `(isearch-fail ((,class (:background ,red :foreground ,bg-main))))
   `(lazy-highlight ((,class (:background ,teal :foreground ,bg-main))))
   `(success ((,class (:foreground ,green :weight bold))))
   `(warning ((,class (:foreground ,orange :weight bold))))
   `(error ((,class (:foreground ,red :weight bold))))
   `(trailing-whitespace ((,class (:background ,red))))
   `(line-number ((,class (:foreground ,fg-faint :background ,bg-main))))
   `(line-number-current-line ((,class (:foreground ,accent :background ,bg-alt :weight bold))))
   `(fill-column-indicator ((,class (:foreground ,border))))
   `(secondary-selection ((,class (:background ,selection))))
   `(tooltip ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(completions-common-part ((,class (:foreground ,accent))))
   `(completions-first-difference ((,class (:foreground ,orange :weight bold))))

   ;; Font-lock faces
   `(font-lock-builtin-face ((,class (:foreground ,cyan))))
   `(font-lock-comment-face ((,class (:foreground ,teal-light :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,teal :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,accent-dark))))
   `(font-lock-doc-face ((,class (:foreground ,teal-light :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,accent :weight bold))))
   `(font-lock-keyword-face ((,class (:foreground ,accent))))
   `(font-lock-negation-char-face ((,class (:foreground ,red))))
   `(font-lock-preprocessor-face ((,class (:foreground ,orange))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,orange))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,violet))))
   `(font-lock-string-face ((,class (:foreground ,orange))))
   `(font-lock-type-face ((,class (:foreground ,accent-dark))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg-main))))
   `(font-lock-warning-face ((,class (:foreground ,red :weight bold))))

   ;; Mode line
   `(mode-line ((,class (:background ,bg-alt :foreground ,fg-main :box (:line-width 1 :color ,border)))))
   `(mode-line-inactive ((,class (:background ,bg-main :foreground ,fg-dim :box (:line-width 1 :color ,border)))))
   `(mode-line-buffer-id ((,class (:foreground ,accent :weight bold))))
   `(mode-line-emphasis ((,class (:foreground ,fg-main :weight bold))))
   `(mode-line-highlight ((,class (:box (:line-width 1 :color ,accent)))))
   `(header-line ((,class (:background ,bg-alt :foreground ,fg-main))))

   ;; Org-mode
   `(org-level-1 ((,class (:foreground ,accent :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,accent-dark :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,orange :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,cyan :weight bold))))
   `(org-level-5 ((,class (:foreground ,teal :weight bold))))
   `(org-level-6 ((,class (:foreground ,fg-main :weight bold))))
   `(org-level-7 ((,class (:foreground ,fg-dim :weight bold))))
   `(org-level-8 ((,class (:foreground ,fg-faint :weight bold))))
   `(org-document-title ((,class (:foreground ,accent :weight bold :height 1.4))))
   `(org-document-info ((,class (:foreground ,fg-dim))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-headline-done ((,class (:foreground ,fg-dim))))
   `(org-date ((,class (:foreground ,accent :underline t))))
   `(org-table ((,class (:foreground ,fg-main))))
   `(org-code ((,class (:foreground ,orange :background ,bg-alt))))
   `(org-verbatim ((,class (:foreground ,accent-dark :background ,bg-alt))))
   `(org-block ((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line ((,class (:foreground ,fg-faint :background ,bg-dim :extend t))))
   `(org-block-end-line ((,class (:foreground ,fg-faint :background ,bg-dim :extend t))))
   `(org-drawer ((,class (:foreground ,fg-faint))))
   `(org-special-keyword ((,class (:foreground ,fg-faint))))
   `(org-tag ((,class (:foreground ,teal))))
   `(org-priority ((,class (:foreground ,orange))))
   `(org-link ((,class (:foreground ,accent :underline t))))
   `(org-checkbox ((,class (:foreground ,accent))))
   `(org-agenda-date ((,class (:foreground ,accent))))
   `(org-agenda-date-today ((,class (:foreground ,orange :weight bold))))
   `(org-agenda-date-weekend ((,class (:foreground ,teal))))
   `(org-scheduled ((,class (:foreground ,fg-main))))
   `(org-scheduled-today ((,class (:foreground ,accent))))
   `(org-scheduled-previously ((,class (:foreground ,orange))))
   `(org-upcoming-deadline ((,class (:foreground ,red))))
   `(org-warning ((,class (:foreground ,red :weight bold))))

   ;; Diff/Ediff
   `(diff-added ((,class (:foreground ,green :background ,diff-add-bg :extend t))))
   `(diff-removed ((,class (:foreground ,red :background ,diff-del-bg :extend t))))
   `(diff-changed ((,class (:foreground ,orange :background ,diff-chg-bg :extend t))))
   `(diff-header ((,class (:foreground ,fg-main :background ,bg-alt))))
   `(diff-file-header ((,class (:foreground ,accent :background ,bg-alt :weight bold))))
   `(diff-hunk-header ((,class (:foreground ,accent :background ,bg-dim))))
   `(diff-indicator-added ((,class (:foreground ,green :weight bold))))
   `(diff-indicator-removed ((,class (:foreground ,red :weight bold))))
   `(diff-indicator-changed ((,class (:foreground ,orange :weight bold))))
   `(diff-refine-added ((,class (:foreground ,green :background "#a8d8c8" :weight bold))))
   `(diff-refine-removed ((,class (:foreground ,red :background "#d8a8a8" :weight bold))))
   `(diff-refine-changed ((,class (:foreground ,orange :background "#d8d0a8" :weight bold))))

   `(ediff-current-diff-A ((,class (:background ,diff-del-bg :extend t))))
   `(ediff-current-diff-B ((,class (:background ,diff-add-bg :extend t))))
   `(ediff-current-diff-C ((,class (:background ,diff-chg-bg :extend t))))
   `(ediff-fine-diff-A ((,class (:background "#d8a8a8" :weight bold))))
   `(ediff-fine-diff-B ((,class (:background "#a8d8c8" :weight bold))))
   `(ediff-fine-diff-C ((,class (:background "#d8d0a8" :weight bold))))
   `(ediff-even-diff-A ((,class (:background ,bg-alt))))
   `(ediff-even-diff-B ((,class (:background ,bg-alt))))
   `(ediff-even-diff-C ((,class (:background ,bg-alt))))
   `(ediff-odd-diff-A ((,class (:background ,bg-dim))))
   `(ediff-odd-diff-B ((,class (:background ,bg-dim))))
   `(ediff-odd-diff-C ((,class (:background ,bg-dim))))

   ;; Magit
   `(magit-section-heading ((,class (:foreground ,accent :weight bold))))
   `(magit-section-highlight ((,class (:background ,bg-alt :extend t))))
   `(magit-branch-local ((,class (:foreground ,accent))))
   `(magit-branch-remote ((,class (:foreground ,green))))
   `(magit-branch-current ((,class (:foreground ,accent :box (:line-width 1 :color ,accent)))))
   `(magit-hash ((,class (:foreground ,fg-dim))))
   `(magit-diff-added ((,class (:foreground ,green :background ,diff-add-bg :extend t))))
   `(magit-diff-removed ((,class (:foreground ,red :background ,diff-del-bg :extend t))))
   `(magit-diff-added-highlight ((,class (:foreground ,green :background ,diff-add-bg :extend t))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red :background ,diff-del-bg :extend t))))
   `(magit-diff-context ((,class (:foreground ,fg-dim :background ,bg-main :extend t))))
   `(magit-diff-context-highlight ((,class (:foreground ,fg-dim :background ,bg-alt :extend t))))
   `(magit-diff-hunk-heading ((,class (:foreground ,fg-main :background ,bg-dim))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,fg-main :background ,selection))))
   `(magit-diff-file-heading ((,class (:foreground ,fg-main :weight bold))))
   `(magit-log-author ((,class (:foreground ,orange))))
   `(magit-log-date ((,class (:foreground ,fg-dim))))

   ;; Git-gutter
   `(git-gutter:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,red :weight bold))))
   `(git-gutter:modified ((,class (:foreground ,orange :weight bold))))
   `(git-gutter-fr:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))))
   `(git-gutter-fr:modified ((,class (:foreground ,orange :weight bold))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,orange)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,accent)))))
   `(flycheck-fringe-error ((,class (:foreground ,red))))
   `(flycheck-fringe-warning ((,class (:foreground ,orange))))
   `(flycheck-fringe-info ((,class (:foreground ,accent))))
   `(flycheck-error-list-error ((,class (:foreground ,red))))
   `(flycheck-error-list-warning ((,class (:foreground ,orange))))
   `(flycheck-error-list-info ((,class (:foreground ,accent))))

   ;; LSP
   `(lsp-face-highlight-read ((,class (:background ,selection))))
   `(lsp-face-highlight-write ((,class (:background ,selection))))
   `(lsp-face-highlight-textual ((,class (:background ,selection))))
   `(lsp-ui-doc-background ((,class (:background ,bg-alt))))
   `(lsp-ui-doc-header ((,class (:foreground ,accent :weight bold))))
   `(lsp-ui-peek-peek ((,class (:background ,bg-alt))))
   `(lsp-ui-peek-list ((,class (:background ,bg-dim))))
   `(lsp-ui-peek-filename ((,class (:foreground ,accent))))
   `(lsp-ui-peek-line-number ((,class (:foreground ,fg-faint))))
   `(lsp-ui-peek-highlight ((,class (:background ,selection))))
   `(lsp-ui-peek-header ((,class (:background ,selection :foreground ,fg-main))))
   `(lsp-ui-peek-footer ((,class (:background ,selection))))
   `(lsp-ui-peek-selection ((,class (:background ,selection))))
   `(lsp-ui-sideline-code-action ((,class (:foreground ,orange))))
   `(lsp-ui-sideline-current-symbol ((,class (:foreground ,accent :weight bold))))
   `(lsp-ui-sideline-symbol ((,class (:foreground ,fg-dim))))

   ;; Corfu (completion)
   `(corfu-default ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(corfu-current ((,class (:background ,selection :foreground ,fg-main))))
   `(corfu-bar ((,class (:background ,accent))))
   `(corfu-border ((,class (:background ,border))))
   `(corfu-annotations ((,class (:foreground ,fg-dim))))
   `(corfu-deprecated ((,class (:foreground ,fg-faint :strike-through t))))

   ;; Vertico
   `(vertico-current ((,class (:background ,selection :extend t))))
   `(vertico-group-title ((,class (:foreground ,accent :weight bold))))
   `(vertico-group-separator ((,class (:foreground ,border :strike-through t))))

   ;; Marginalia
   `(marginalia-key ((,class (:foreground ,accent))))
   `(marginalia-documentation ((,class (:foreground ,fg-dim :slant italic))))
   `(marginalia-file-name ((,class (:foreground ,fg-main))))
   `(marginalia-file-priv-dir ((,class (:foreground ,accent))))
   `(marginalia-file-priv-exec ((,class (:foreground ,green))))
   `(marginalia-file-priv-link ((,class (:foreground ,magenta))))
   `(marginalia-file-priv-read ((,class (:foreground ,fg-main))))
   `(marginalia-file-priv-write ((,class (:foreground ,orange))))

   ;; Consult
   `(consult-preview-cursor ((,class (:background ,selection))))
   `(consult-preview-line ((,class (:background ,bg-alt :extend t))))
   `(consult-file ((,class (:foreground ,fg-main))))
   `(consult-bookmark ((,class (:foreground ,accent))))

   ;; Orderless
   `(orderless-match-face-0 ((,class (:foreground ,accent :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,orange :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,accent-dark :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,magenta :weight bold))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,accent))))
   `(which-key-command-description-face ((,class (:foreground ,fg-main))))
   `(which-key-group-description-face ((,class (:foreground ,orange))))
   `(which-key-separator-face ((,class (:foreground ,fg-faint))))

   ;; Company (fallback if not using corfu)
   `(company-tooltip ((,class (:background ,bg-alt :foreground ,fg-main))))
   `(company-tooltip-selection ((,class (:background ,selection))))
   `(company-tooltip-common ((,class (:foreground ,accent :weight bold))))
   `(company-tooltip-annotation ((,class (:foreground ,fg-dim))))
   `(company-scrollbar-bg ((,class (:background ,bg-dim))))
   `(company-scrollbar-fg ((,class (:background ,accent))))
   `(company-preview ((,class (:foreground ,fg-faint))))
   `(company-preview-common ((,class (:foreground ,accent))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,accent :weight bold))))
   `(dired-symlink ((,class (:foreground ,magenta))))
   `(dired-marked ((,class (:foreground ,orange :weight bold))))
   `(dired-flagged ((,class (:foreground ,red :weight bold))))
   `(dired-header ((,class (:foreground ,accent :weight bold))))
   `(dired-ignored ((,class (:foreground ,fg-faint))))
   `(dired-perm-write ((,class (:foreground ,orange))))

   ;; Neotree
   `(neo-banner-face ((,class (:foreground ,accent :weight bold))))
   `(neo-header-face ((,class (:foreground ,fg-main :weight bold))))
   `(neo-root-dir-face ((,class (:foreground ,accent :weight bold))))
   `(neo-dir-link-face ((,class (:foreground ,accent))))
   `(neo-file-link-face ((,class (:foreground ,fg-main))))
   `(neo-expand-btn-face ((,class (:foreground ,accent))))

   ;; Dashboard
   `(dashboard-banner-logo-title ((,class (:foreground ,accent :weight bold))))
   `(dashboard-heading ((,class (:foreground ,accent :weight bold))))
   `(dashboard-items-face ((,class (:foreground ,fg-main))))

   ;; Evil
   `(evil-ex-info ((,class (:foreground ,red))))
   `(evil-ex-substitute-matches ((,class (:background ,red :foreground ,bg-main))))
   `(evil-ex-substitute-replacement ((,class (:background ,green :foreground ,bg-main))))

   ;; Doom-modeline
   `(doom-modeline-bar ((,class (:background ,accent))))
   `(doom-modeline-bar-inactive ((,class (:background ,bg-alt))))
   `(doom-modeline-buffer-file ((,class (:foreground ,fg-main :weight bold))))
   `(doom-modeline-buffer-modified ((,class (:foreground ,orange :weight bold))))
   `(doom-modeline-buffer-path ((,class (:foreground ,fg-dim))))
   `(doom-modeline-evil-normal-state ((,class (:foreground ,accent :weight bold))))
   `(doom-modeline-evil-insert-state ((,class (:foreground ,green :weight bold))))
   `(doom-modeline-evil-visual-state ((,class (:foreground ,orange :weight bold))))
   `(doom-modeline-evil-replace-state ((,class (:foreground ,red :weight bold))))
   `(doom-modeline-evil-emacs-state ((,class (:foreground ,cyan :weight bold))))
   `(doom-modeline-evil-motion-state ((,class (:foreground ,magenta :weight bold))))
   `(doom-modeline-evil-operator-state ((,class (:foreground ,accent-dark :weight bold))))
   `(doom-modeline-info ((,class (:foreground ,green))))
   `(doom-modeline-warning ((,class (:foreground ,orange))))
   `(doom-modeline-urgent ((,class (:foreground ,red))))
   `(doom-modeline-project-dir ((,class (:foreground ,accent :weight bold))))
   `(doom-modeline-project-root-dir ((,class (:foreground ,accent))))
   `(doom-modeline-lsp-success ((,class (:foreground ,green))))
   `(doom-modeline-lsp-warning ((,class (:foreground ,orange))))
   `(doom-modeline-lsp-error ((,class (:foreground ,red))))

   ;; Avy
   `(avy-lead-face ((,class (:background ,accent :foreground ,bg-main :weight bold))))
   `(avy-lead-face-0 ((,class (:background ,orange :foreground ,bg-main :weight bold))))
   `(avy-lead-face-1 ((,class (:background ,teal :foreground ,bg-main :weight bold))))
   `(avy-lead-face-2 ((,class (:background ,cyan :foreground ,bg-main :weight bold))))

   ;; Undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,fg-main))))
   `(undo-tree-visualizer-current-face ((,class (:foreground ,accent :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))))
   `(undo-tree-visualizer-register-face ((,class (:foreground ,orange))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,accent))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,accent-dark))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,teal))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,magenta))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,violet))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,fg-dim))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,fg-main))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red :weight bold))))

   ;; Treemacs
   `(treemacs-root-face ((,class (:foreground ,accent :weight bold :height 1.1))))
   `(treemacs-directory-face ((,class (:foreground ,accent))))
   `(treemacs-file-face ((,class (:foreground ,fg-main))))
   `(treemacs-git-added-face ((,class (:foreground ,green))))
   `(treemacs-git-modified-face ((,class (:foreground ,orange))))
   `(treemacs-git-untracked-face ((,class (:foreground ,fg-dim))))
   `(treemacs-git-ignored-face ((,class (:foreground ,fg-faint))))

   ;; Term
   `(term ((,class (:foreground ,fg-main :background ,bg-main))))
   `(term-color-black ((,class (:foreground ,border :background ,border))))
   `(term-color-red ((,class (:foreground ,red :background ,red))))
   `(term-color-green ((,class (:foreground ,green :background ,green))))
   `(term-color-yellow ((,class (:foreground ,orange :background ,orange))))
   `(term-color-blue ((,class (:foreground ,accent :background ,accent))))
   `(term-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
   `(term-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
   `(term-color-white ((,class (:foreground ,fg-main :background ,fg-main))))

   ;; Web-mode
   `(web-mode-html-tag-face ((,class (:foreground ,accent))))
   `(web-mode-html-tag-bracket-face ((,class (:foreground ,fg-dim))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,orange))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,accent-dark))))
   `(web-mode-doctype-face ((,class (:foreground ,fg-faint))))
   `(web-mode-comment-face ((,class (:foreground ,teal-light :slant italic))))
   `(web-mode-css-selector-face ((,class (:foreground ,accent))))
   `(web-mode-css-property-name-face ((,class (:foreground ,fg-main))))
   `(web-mode-css-string-face ((,class (:foreground ,orange))))

   ;; Markdown
   `(markdown-header-face-1 ((,class (:foreground ,accent :weight bold :height 1.3))))
   `(markdown-header-face-2 ((,class (:foreground ,accent-dark :weight bold :height 1.2))))
   `(markdown-header-face-3 ((,class (:foreground ,orange :weight bold :height 1.1))))
   `(markdown-header-face-4 ((,class (:foreground ,cyan :weight bold))))
   `(markdown-header-face-5 ((,class (:foreground ,teal :weight bold))))
   `(markdown-header-face-6 ((,class (:foreground ,fg-main :weight bold))))
   `(markdown-code-face ((,class (:foreground ,orange :background ,bg-alt))))
   `(markdown-inline-code-face ((,class (:foreground ,orange :background ,bg-alt))))
   `(markdown-pre-face ((,class (:foreground ,orange :background ,bg-alt))))
   `(markdown-link-face ((,class (:foreground ,accent))))
   `(markdown-url-face ((,class (:foreground ,fg-dim :underline t))))
   `(markdown-bold-face ((,class (:foreground ,fg-main :weight bold))))
   `(markdown-italic-face ((,class (:foreground ,fg-main :slant italic))))

   ;; mu4e
   `(mu4e-header-highlight-face ((,class (:background ,selection :extend t))))
   `(mu4e-unread-face ((,class (:foreground ,accent :weight bold))))
   `(mu4e-flagged-face ((,class (:foreground ,orange))))
   `(mu4e-replied-face ((,class (:foreground ,green))))
   `(mu4e-forwarded-face ((,class (:foreground ,cyan))))
   `(mu4e-header-key-face ((,class (:foreground ,accent :weight bold))))
   `(mu4e-header-value-face ((,class (:foreground ,fg-main))))
   `(mu4e-special-header-value-face ((,class (:foreground ,orange))))
   `(mu4e-link-face ((,class (:foreground ,accent :underline t))))
   `(mu4e-contact-face ((,class (:foreground ,accent))))
   `(mu4e-compose-separator-face ((,class (:foreground ,border))))

   ;; Show-paren
   `(show-paren-match ((,class (:background ,selection :foreground ,fg-main :weight bold))))
   `(show-paren-mismatch ((,class (:background ,red :foreground ,bg-main :weight bold))))

   ;; Whitespace
   `(whitespace-space ((,class (:foreground ,bg-dim))))
   `(whitespace-tab ((,class (:foreground ,bg-dim))))
   `(whitespace-newline ((,class (:foreground ,bg-dim))))
   `(whitespace-trailing ((,class (:background ,red-light))))
   `(whitespace-line ((,class (:background ,bg-alt :foreground ,orange))))

   ;; Fancy battery
   `(fancy-battery-charging ((,class (:foreground ,green))))
   `(fancy-battery-discharging ((,class (:foreground ,orange))))
   `(fancy-battery-critical ((,class (:foreground ,red :weight bold))))

   ;; YASnippet
   `(yas-field-highlight-face ((,class (:background ,selection))))

   ;; Erc
   `(erc-nick-default-face ((,class (:foreground ,accent :weight bold))))
   `(erc-current-nick-face ((,class (:foreground ,orange :weight bold))))
   `(erc-my-nick-face ((,class (:foreground ,orange :weight bold))))
   `(erc-notice-face ((,class (:foreground ,fg-dim))))
   `(erc-input-face ((,class (:foreground ,fg-main))))
   `(erc-timestamp-face ((,class (:foreground ,teal))))
   `(erc-prompt-face ((,class (:foreground ,accent :weight bold))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'seashells-light)

;;; seashells-light-theme.el ends here
