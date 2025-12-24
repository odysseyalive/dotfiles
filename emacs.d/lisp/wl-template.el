;;; ~/.wl --- Wanderlust Configuration Template -*-emacs-lisp-*-

;; Copy this file to ~/.wl and edit with your settings
;; Then create ~/.authinfo.gpg with your credentials

;;; Your Identity
(setq wl-from "Your Name <you@example.com>"
      user-mail-address "you@example.com"
      user-full-name "Your Name")

;;; IMAP Server (incoming mail)
(setq elmo-imap4-default-server "imap.example.com"
      elmo-imap4-default-user "you@example.com"
      elmo-imap4-default-port 993
      elmo-imap4-default-stream-type 'ssl)

;;; SMTP Server (outgoing mail)
(setq wl-smtp-posting-server "smtp.example.com"
      wl-smtp-posting-user "you@example.com"
      wl-smtp-posting-port 587
      wl-smtp-connection-type 'starttls
      wl-smtp-authenticate-type "plain")

;;; Folder names (adjust for your mail server)
;; Gmail example:
;; (setq wl-default-folder "%INBOX"
;;       wl-draft-folder "%[Gmail]/Drafts"
;;       wl-trash-folder "%[Gmail]/Trash"
;;       wl-spam-folder "%[Gmail]/Spam"
;;       wl-sent-folder "%[Gmail]/Sent Mail")

;; Standard IMAP:
(setq wl-default-folder "%INBOX"
      wl-draft-folder "%Drafts"
      wl-trash-folder "%Trash"
      wl-spam-folder "%Spam"
      wl-sent-folder "%Sent")

;;; Example: Multiple accounts
;; Uncomment and modify for additional accounts
;;
;; (setq wl-user-mail-address-list
;;       '("work@company.com" "personal@gmail.com"))
;;
;; (setq wl-template-alist
;;       '(("work"
;;          (wl-from . "Your Name <work@company.com>")
;;          (wl-smtp-posting-server . "smtp.company.com"))
;;         ("personal"
;;          (wl-from . "Your Name <personal@gmail.com>")
;;          (wl-smtp-posting-server . "smtp.gmail.com"))))

;;; ~/.wl ends here
