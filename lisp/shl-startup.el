;;; shl-startup.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package diminish
;;   :ensure t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(use-package gcmh
  :ensure t
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024))
  (add-hook 'after-init-hook (lambda ()
                               (gcmh-mode))))



(setq jit-lock-defer-time 0)

(provide 'shl-startup)
;;; shl-startup.el ends here
