;;; init-functions.el --- Custom Functions -*- lexical-binding: t -*-

;;; Commentary:
;; Custom utility functions.

;;; Code:

;; Copy file path to clipboard
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

;; Revert buffer without confirmation
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; Force kill current buffer
(defun kill-this-buffer ()
  "Force kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;; Reinstall all packages
(defun package-reinstall-all-activated-packages ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (byte-recompile-directory package-user-dir nil 'force))

;; Fix PHP short open tags
(defun fix-short-open-tags ()
  "Replaces all Short Open Tags in a project's PHP code."
  (interactive)
  (shell-command "find . \\( -type d -name .git -prune \\) -o -type f -iname \"*.php\" -print0 | xargs -0 -I{} sed -i -r 's/(<\\?)([^a-zA-Z=]|$)/\\1php \\2/g' '{}'")
  (shell-command "find . \\( -type d -name .git -prune \\) -o -type f -iname \"*.css\" -print0 | xargs -0 -I{} sed -i -r 's/(<\\?)([^a-zA-Z=]|$)/\\1php \\2/g' '{}'")
  (shell-command "find . \\( -type d -name .git -prune \\) -o -type f -iname \"*.js\" -print0 | xargs -0 -I{} sed -i -r 's/(<\\?)([^a-zA-Z=]|$)/\\1php \\2/g' '{}'")
  (shell-command "find . \\( -type d -name .git -prune \\) -o -type f -iname \"*.tpl\" -print0 | xargs -0 -I{} sed -i -r 's/(<\\?)([^a-zA-Z=]|$)/\\1php \\2/g' '{}'"))

(provide 'init-functions)
;;; init-functions.el ends here
