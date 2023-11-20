;; -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default
 ad-redefinition-action 'accept                     ; Silence warnings for redifinition
 display-time-default-load-average nil              ; Don't display load average
 fill-column 80                                     ; Set width for automatic line breaks
 help-window-select t                               ; Focus new help windows when opened
 indent-tabs-mode nil                               ; Prefer spaces over tabs
 kill-ring-max 128                                  ; Maximum length of kill ring
 load-prefer-newer t                                ; Prefer the newest veriosion of a file
 mark-ring-max 128                                  ; Maximum length of mark ring
 read-process-output-max (* 1024 1024)              ; Increase amount of data read from the process
 select-enable-clipboard t                          ; Merge system and emacs clipboard
 tab-width 4                                        ; Set width for tabs
 vc-follow-symlinks t)                              ; Always follow symlinks

(setq                                               
 indicate-empty-lines nil                           ; No empty line indicators
 cursor-in-non-selected-windows nil                 ; No cursor in inactive windows
 default-major-mode 'org-mode                       ; Use org-mode as default major mode
 font-lock-maximum-decoration nil                   ; Moderate font lock
 font-lock-maximum-size nil                         ; No limit on font lock
 auto-fill-mode nil                                 ; No line break space points
 confirm-non-existent-file-or-buffer nil            ; No confirmation for visiting non-existent files
 window-min-height 1                                ; Minimum window height
 native-comp-async-report-warnings-errors 'silent   ; Disable native comp warnings
 native-compile-prune-cache t)

(column-number-mode 1)                              ; Show the column number
(fset 'yes-or-no-p 'y-or-n-p)                       ; Replace yes/no prompts with y/n
(set-default-coding-systems 'utf-8)                 ; Default to utf-8 encoding
(show-paren-mode 1)                                 ; Show the parent

(add-hook 'prog-mode-hook #'hl-line-mode)           ; Highlight lines in prog-mode
(add-hook 'text-mode-hook #'hl-line-mode)           ; Highlight lines in text-mode  
(add-hook 'org-mode-hook #'hl-line-mode)            ; Highlight lines in org-mode  


;; Ensure that Emacs does not clutter all of our directories
(make-directory "~/.emacs.d/emacs_backups/" t)
(make-directory "~/.emacs.d/emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/emacs_backups/")))
(setq backup-by-copying t)

;; Ensure that we are using the same PATH as in terminal
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(provide 'shl-defaults)
;; shl-defaults.el ends here
