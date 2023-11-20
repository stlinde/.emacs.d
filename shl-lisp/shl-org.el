;; -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure org-contrib
  :hook ((auto-save . org-save-all-org-buffers)
         (org-mode . visual-line-mode))
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :config
  ;; General Settings
  (setq-default org-directory "~/hempellinde"
                org-default-notes-file "~/hempellinde/inbox.org"
                org-tags-column 1                     ; Tags next to header line
                org-emphasis-hide-markers t           ; Hide markers
                org-cycle-separator-lines 2           ; Number of empty lines between sections
                org-use-property-inheritance t        ; Properties are inherited
                org-indent-indentation-per-level 2    ; Indentation per level
                org-fontify-quote-and-verse-blocks t  ; Specific face for quote and verse blocks
                org-return-follows-link nil           ; Follow links when hitting return
                org-indirect-buffer-display 'other-window ; Tab on a task to expand it in a new window
                org-startup-indented t              ; Start with indent on
                org-refile-use-outline-path 'file     ; Show as file path
                org-outline-path-complete-in-steps nil ; Don't refile in steps
                org-log-done 'time                    ; Log time when changed to done
                org-log-into-drawer t)

  ;; Org-babel Settings
  (setq-default org-src-fontify-natively t         ; Fontify code in code blocks.
              org-adapt-indentation nil          ; Adaptive indentation
              org-src-tab-acts-natively t        ; Tab acts as in source editing
              org-confirm-babel-evaluate nil     ; No confirmation before executing code
              org-edit-src-content-indentation 0 ; No relative indentation for code blocks
              org-fontify-whole-block-delimiter-line t)) ; Fontify whole block

(use-package org-modern
  :ensure t
  :init (global-org-modern-mode))

(provide 'shl-org)
;; shl-org.el ends here
