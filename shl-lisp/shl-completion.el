;; -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/*.el"))
  :ensure t
  :config
  (setq corfu-auto t                   ; Enable auto completion
	    corfu-auto-delay 0.25           ; Delay before auto-completion shows up
	    corfu-auto-prefix 1            ; Characters needed before auto-completion
	    corfu-popupinfo-delay 0.5     ; Delay before docs show up
        corfu-quit-no-match t          ; Quit when there is no match
        corfu-preview-current nil)     ; Don't preview current completion candidate
  :init
  (corfu-popupinfo-mode)
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
	    ("C-y" . corfu-complete)
        ("TAB" . nil)
	    ("RET" . nil)
        ("C-p" . corfu-previous)))

(use-package cape
  :demand t
  :bind (("C-c . p" . completion-at-point)
         ("C-c . t" . complete-tag)
         ("C-c . d" . cape-dabbrev)
         ("C-c . h" . cape-history)
         ("C-c . f" . cape-file)
         ("C-c . k" . cape-keyword)
         ("C-c . s" . cape-symbol)
         ("C-c . a" . cape-abbrev)
         ("C-c . l" . cape-line)
         ("C-c . w" . cape-dict)
         ("C-c . \\" . cape-tex)
         ("C-c . _" . cape-tex)
         ("C-c . ^" . cape-tex)
         ("C-c . &" . cape-sgml)
         ("C-c . r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(provide 'shl-completion)
;;; shl-completion.el ends here
