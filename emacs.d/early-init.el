;;; early-init.el --- Early Init -*- lexical-binding: t -*-

;;; Commentary:
;; Early initialization for fast startup.
;; This runs before init.el and package.el.

;;; Code:

;; Defer garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Prevent package.el from loading before init
(setq package-enable-at-startup nil)

;; Disable UI elements early (before frame is created)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set frame parameters early
(push '(alpha . (97 . 97)) default-frame-alist)

;; Don't inhibit display - causes issues with some configurations
;; The GC threshold optimization provides the main speedup anyway

;; Prevent unwanted runtime native compilation
(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation nil)
  (setq native-comp-async-report-warnings-errors nil))

;; Disable file-name-handler-alist temporarily during startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Increase read-process-output-max for LSP performance
(setq read-process-output-max (* 1024 1024))

;;; early-init.el ends here
