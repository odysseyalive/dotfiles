;;; init-keybindings.el --- Keybindings Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; All keybindings using general.el.
;; Leader key: , (comma) for Evil modes, M-, for Emacs/Insert modes.
;; Updated to use modern packages (Vertico/Consult/Corfu/Avy).

;;; Code:

;; Leader key configuration
(setq gleader ",")
(setq gleader-non "M-,")

(use-package general
  :ensure t
  :config
  (general-evil-setup)
  (general-override-mode)

  ;; Single Key Functions (YADR Remnants)
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "," '(execute-extended-command :which-key "Search for Command")
   ";" '(evil-commentary :which-key "Un/Comment")
   "[" '(shrink-window-horizontally :which-key "Contract Window")
   "]" '(enlarge-window-horizontally :which-key "Expand Window")
   "-" '(shrink-window :which-key "Shrink Window")
   "=" '(enlarge-window :which-key "Grow Window")
   "e" '(flycheck-list-errors-toggle :which-key "Error List")
   "h" '(call-dashboard :which-key "Dashboard")
   "j" '(avy-goto-char :which-key "Jump to Character")
   "m" '(deer :which-key "Browse Current Directory")
   "n" '(neotree-toggle :which-key "Browse Project Directory")
   "q" '(kill-this-buffer :which-key "Quit Buffer")
   "Q" '(save-buffers-kill-emacs :which-key "Quit Emacs")
   "x" '(next-buffer :which-key "Next Buffer")
   "z" '(previous-buffer :which-key "Previous Buffer")
   "U" '(package-reinstall-all-activated-packages :which-key "Update All Packages")
   "/" '(consult-ripgrep :which-key "Search in Project"))


  ;; Applications
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "a"  '(nil :which-key "Applications")
   "ah" '(info-emacs-manual :which-key "Read the Emacs Manual")
   "ai" '(erc :which-key "IRC")
   "at" '(multi-term :which-key "Terminal")
   "am" '(wl :which-key "Email")
   "aw" '(w3m-browse-url :which-key "Browse Web"))


  ;; Buffer Functions
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "b"  '(nil :which-key "Buffers")
   "bb" '(consult-buffer :which-key "List/Create Buffers")
   "bc" '(my-put-file-name-on-clipboard :which-key "Copy File Path")
   "bf" '(find-file :which-key "Open File")
   "bn" '(evil-buffer-new :which-key "Create New Buffer")
   "bp" '(consult-projectile-find-file :which-key "Find File")
   "bq" '(kill-this-buffer :which-key "Quit Buffer")
   "br" '(revert-buffer-no-confirm :which-key "Refresh Buffer")
   "bs" '(save-buffer :which-key "Save Buffer"))


  ;; DAP Functions
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "d"  '(nil :which-key "Dap-Mode (DBGp)")
   "da" '(dap-breakpoint-delete-all :which-key "Delete all Breakpoints")
   "db" '(dap-breakpoint-toggle :which-key "Toggle Breakpoint")
   "dc" '(dap-continue :which-key "Continue to Breakpoint")
   "dd" '(dap-debug :which-key "Start Dap-Mode")
   "di" '(dap-step-in :which-key "Step into")
   "dl" '(dap-ui-breakpoints :which-key "List all Breakpoints")
   "dn" '(dap-next :which-key "Next Line")
   "do" '(dap-step-out :which-key "Step out")
   "dp" '(dap-php-setup :which-key "Install vscode-php-debug plugin")
   "dv" '(dap-ui-locals :which-key "View Context")
   "dx" '(terminate-dap-mode :which-key "Stop Dap-Mode"))


  ;; LSP Mode Navigation
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "f"  '(nil :which-key "Find")
   "fd" '(lsp-find-definition :which-key "Find Definition")
   "fD" '(consult-find :which-key "Find in Directory")
   "ff" '(vertico-repeat :which-key "Resume Last Find")
   "fi" '(lsp-find-implementation :which-key "Find Implementation")
   "fm" '(consult-imenu :which-key "Menu")
   "fp" '(consult-projectile-find-file :which-key "Projectile: Find File")
   "fr" '(lsp-find-references :which-key "Find Reference")
   "fs" '(lsp-ui-doc-toggle :which-key "Show Documentation"))


  ;; Word Functions
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "g"  '(nil :which-key "Grammar")
   "ga" '(powerthesaurus-lookup-antonyms-dwim :which-key "See Antonyms")
   "ge" '(emojify-insert-emoji :which-key "Insert Emoji")
   "gi" '(nerd-icons-insert :which-key "Insert Icon")
   "gs" '(flyspell-mode :which-key "View Spelling Errors")
   "gc" '(flyspell-auto-correct-word :which-key "Correct Spelling")
   "gd" '(wordnut-lookup-current-word :which-key "Word Definition & Thesaurus")
   "gw" '(wordnut-search :which-key "Word Lookup"))


  ;; Line Functions
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "l"  '(nil :which-key "Line Manipulation")
   "la" '(toggle-artist-mode :which-key "Draw With Cursor [Emacs Mode]")
   "lb" '(beautify-code :which-key "Beautify Code")
   "lc" '(rainbow-mode :which-key "Toggle Color Codes")
   "le" '(epa-encrypt-region :which-key "Encrypt Selection")
   "ld" '(epa-decrypt-region :which-key "Decrypt Selection")
   "li" '(aggressive-indent-mode :which-key "Toggle Aggressive Indenting")
   "ln" '(display-line-numbers-mode :which-key "View Line Numbers")
   "lm" '(term-toggle-mode :which-key "(Terminal Only) Toggle Line/Character Mode")
   "lr" '(display-line-numbers-mode :which-key "Toggle Relative Numbers")
   "ls" '(delete-trailing-whitespace :which-key "Delete Trailing Whitespaces")
   "lt" '(toggle-php-flavor-mode :which-key "Toggle PHP/Web Mode Highlighting")
   "lw" '(toggle-truncate-lines :which-key "Toggle Line Wrapping"))


  ;; Org Mode Controls
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "o"  '(nil :which-key "Org-Mode")
   "oo" '(org-agenda :which-key "Agenda Controls")
   "oa" '(org-ctrl-c-ctrl-c :which-key "Activate (C-c)")
   "od" '(org-deadline :which-key "Schedule Deadline")
   "oc" '(org-evaluate-time-range :which-key "Recompute Clock")
   "ok" '(org-schedule :which-key "Schedule Task")
   "og" '(org-set-tags-command :which-key "Set Tag")
   "ol" '(org-clock-display :which-key "Display Clocks")
   "oj" '(org-clock-goto :which-key "Jump to Running Task")
   "ot" '(org-clock-in :which-key "Start Task")
   "or" '(better-agenda-view :which-key "Agenda Report")
   "os" '(org-clock-out :which-key "Stop Task")
   "ox" '(org-clock-cancel :which-key "Cancel Clock"))


  ;; Project Functions
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "p"  '(nil :which-key "Projects")
   "p]" '(git-gutter:previous-hunk :which-key "Go to previous modification")
   "p[" '(git-gutter:next-hunk :which-key "Go to next modification")
   "pd" '(consult-ripgrep :which-key "Search in Directory")
   "ph" '(magit-log-buffer-file :which-key "History of Buffer")
   "pf" '(consult-projectile-find-file :which-key "Fuzzy File (CtrlP)")
   "pP" '(consult-projectile :which-key "Open Recent Project")
   "pp" '(vertico-repeat :which-key "Resume Last Search")
   "pr" '(consult-recent-file :which-key "Open Recent Project File")
   "ps" '(consult-ripgrep :which-key "Search in Project")
   "pm" '(magit-status :which-key "Git Status")
   "pw" '(consult-line :which-key "Search in Buffer"))


  ;; Lisp Debug Functions
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "s"  '(nil :which-key "Script")
   "sa" '(eval-last-sexp :which-key "Evaluate Statement")
   "sb" '(eval-buffer :which-key "Evaluate Buffer")
   "sc" '(eval-defun :which-key "Evaluate Function")
   "se" '(eval-print-last-sexp :which-key "Evaluate and Print Statement")
   "sf" '(debug-on-entry :which-key "Debug Function")
   "sl" '(load-file :which-key "load e-lisp file")
   "sp" '(emacs-lisp-byte-compile-and-load :which-key "load e-lisp file")
   "ss" '(scratch :which-key "Open Scratch Pad")
   "sv" '(debug-on-variable-change :which-key "Debug Variable")
   "sw" '(cancel-debug-on-variable-change :which-key "Cancel Variable Debugging")
   "sx" '(cancel-debug-on-entry :which-key "Cancel Function Debugging"))


  ;; Misc Toggles
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "t"  '(nil :which-key "Toggles")
   "tt" '(toggle-light-dark-theme :which-key "Toggle Dark/Light Theme"))


  ;; Window Functions
  (general-define-key
   :prefix gleader
   :non-normal-prefix gleader-non
   :states '(normal visual motion insert emacs)
   :keymaps 'override
   "w"  '(nil :which-key "Windows")
   "wb" '(balance-windows :which-key "All Windows Match Size")
   "wd" '(delete-window :which-key "Kill Window")
   "wc" '(delete-other-windows :which-key "Clear All but Current")
   "wf" '(toggle-frame-fullscreen :which-key "Toggle Full Screen")
   "wh" '(evil-window-left :which-key "Select Window Left")
   "wj" '(evil-window-down :which-key "Select Window Above")
   "wk" '(evil-window-up :which-key "Select Window Below")
   "wl" '(evil-window-right :which-key "Select Window Right")
   "wH" '(evil-window-move-far-left :which-key "Move Window Left")
   "wJ" '(evil-window-move-very-bottom :which-key "Move Window Up")
   "wK" '(evil-window-move-very-top :which-key "Move Window Down")
   "wL" '(evil-window-move-far-right :which-key "Move Window Right")
   "wn" '(other-frame :which-key "Switch Screens")
   "wo" '(other-window :which-key "Select Next Window")
   "wp" '(make-frame :which-key "Pop Out to Screen")
   "ws" '(split-window-below :which-key "Horizontal Split")
   "wv" '(split-window-right :which-key "Vertical Split")
   "wx" '(delete-frame :which-key "Close Screen")))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
