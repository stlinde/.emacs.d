;;; shl-defaults.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (or (memq window-system '(mac ns x pgtk))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))

(setq-default bookmark-default-file (locate-user-emacs-file ".bookmarks.el")
              use-short-answers t
	      buffer-menu-max-size 30
	      case-fold-search t
	      column-number-mode t
	      indent-tabs-mode nil
	      create-lockfiles nil
	      auto-save-default nil
	      make-backup-files nil
	      vc-make-backup-files nil
	      save-interprogram-paste-before-kill t
	      scroll-preserve-screen-position 'always
              truncate-lines nil
              truncate-partial-width-windows nil)

;; Speed up font rendering for special characters
;; @see https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
(setq inhibit-compacting-font-caches t)

;; GUI Frames
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Savehist
(savehist-mode 1)

;; Turn of bell
(setq ring-bell-function 'ignore)

(use-package window
  :ensure nil
  :config
  (defun hsplit-last-buffer ()
    "Focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun vsplit-last-buffer ()
    "Focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1))
  (global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
  (global-set-key (kbd "C-x 3") 'hsplit-last-buffer))

(add-hook 'after-init-hook 'delete-selection-mode)

;; Automatically revert buffers when file changes on disk
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
;; (with-eval-after-load 'autorevert
;;   (diminish 'auto-revert-mode))

;; Highlighted region is highlighted with the 'region' face
(add-hook 'after-init-hook 'transient-mark-mode)

(provide 'shl-defaults)
;;; shl-defaults.el ends here
