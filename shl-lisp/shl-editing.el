;; -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package avy
  :ensure t
  :bind (("M-j" . avy-goto-char-timer))
  :config
  (setq avy-enter-times-out t
        avy-timeout-seconds 0.25))

(provide 'shl-editing)
;;; shl-editing.el ends here
