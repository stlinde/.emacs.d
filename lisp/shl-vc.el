;;; shl-vc.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch))

;; Good commit style
(use-package git-commit
  :ensure t)

(provide 'shl-vc)
;;; shl-vc.el ends here
