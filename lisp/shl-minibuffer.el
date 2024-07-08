;;; shl-minibuffer.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vertico
  :ensure t
  :config
  (vertico-mode))


  ;; Embark
(use-package embark
  :ensure t
  :config
  (global-set-key (kbd "C-.") 'embark-act)
  (global-set-key (kbd "M-.") 'embark-dwim))

  ;; Consult
(use-package consult
  :ensure t
  :config
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line))

(use-package embark-consult
  :ensure t
  :config
  (with-eval-after-load 'embark
    (require 'embark-consult)
    (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))

  ;; Marginalia
(use-package marginalia
  :ensure t
  :config
  (add-hook 'after-init-hook 'marginalia-mode))

(use-package which-key
  :ensure t
  :config
  (add-hook 'after-init-hook 'which-key-mode)
  (setq-default which-key-idle-delay 0.3))

(provide 'shl-minibuffer)
;;; shl-minibuffer.el ends here
