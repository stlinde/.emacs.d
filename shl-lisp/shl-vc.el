;; -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(provide 'shl-vc)
;; shl-vc.el ends here
