;;; init-mail.el --- Email Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Wanderlust email client - fast IMAP with simple setup.
;;
;; Quick Setup:
;; 1. Copy ~/.yadrlite/emacs.d/lisp/wl-template.el to ~/.wl
;; 2. Edit ~/.wl with your email settings
;; 3. Create ~/.authinfo.gpg with credentials (see below)
;; 4. Run M-x wl
;;
;; ~/.authinfo.gpg format:
;;   machine imap.example.com login you@example.com password SECRET port 993
;;   machine smtp.example.com login you@example.com password SECRET port 587

;;; Code:

(use-package wanderlust
  :ensure t
  :commands (wl wl-other-frame)
  :init
  ;; Directory for Wanderlust data
  (setq elmo-msgdb-directory "~/.emacs.d/elmo"
        wl-temporary-file-directory "~/.emacs.d/wl-tmp"
        wl-init-file "~/.wl"
        wl-folders-file "~/.folders")

  :config
  ;; IMAP settings (override in ~/.wl)
  (setq elmo-imap4-default-stream-type 'ssl
        elmo-imap4-default-port 993
        elmo-imap4-use-modified-utf7 t)

  ;; SMTP settings
  (setq wl-smtp-connection-type 'starttls
        wl-smtp-posting-port 587
        wl-smtp-authenticate-type "plain")

  ;; Folders
  (setq wl-default-folder "%INBOX"
        wl-draft-folder "%Drafts"
        wl-trash-folder "%Trash"
        wl-fcc-force-as-read t)

  ;; Summary display
  (setq wl-summary-width nil
        wl-summary-line-format "%T%P%Y-%M-%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s"
        wl-thread-indent-level 2
        wl-thread-have-younger-brother-str "├─"
        wl-thread-youngest-child-str "└─"
        wl-thread-vertical-str "│ "
        wl-thread-horizontal-str "─"
        wl-thread-space-str "  ")

  ;; Message display
  (setq mime-view-mailcap-files nil
        wl-message-ignored-field-list '("^.*:")
        wl-message-visible-field-list
        '("^From:" "^To:" "^Cc:" "^Date:" "^Subject:"))

  ;; Auto-fill for composing
  (add-hook 'wl-mail-setup-hook
            (lambda ()
              (auto-fill-mode 1)
              (setq fill-column 72)))

  ;; Mark sent messages as read
  (setq wl-fcc-force-as-read t)

  ;; Prefer plain text, but allow images
  (setq mime-view-type-subtype-score-alist
        '(((text . plain) . 4)
          ((text . enriched) . 3)
          ((text . html) . 2)
          ((text . richtext) . 1)))

  ;; Display images inline
  (setq mime-view-image-type-list '(png jpeg gif)
        mime-view-inline-image t
        wl-message-use-inline-image t)

  ;; Don't ask for confirmation on large folders
  (setq elmo-folder-update-confirm nil
        elmo-folder-update-threshold nil)

  ;; Offline mode disabled (we want online IMAP)
  (setq wl-plugged t
        elmo-imap4-use-cache nil))

;; SEMI/FLIM for MIME (required by Wanderlust)
(use-package semi
  :ensure t)

;; Evil keybindings for Wanderlust
(with-eval-after-load 'evil
  (evil-set-initial-state 'wl-folder-mode 'emacs)
  (evil-set-initial-state 'wl-summary-mode 'emacs)
  (evil-set-initial-state 'wl-message-mode 'motion)
  (evil-set-initial-state 'wl-draft-mode 'insert)
  (evil-set-initial-state 'mime-view-mode 'motion))

(provide 'init-mail)
;;; init-mail.el ends here
