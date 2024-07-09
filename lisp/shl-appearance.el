;;; shl-appearance.el --- -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;; Don't ask if theme is safe
(setopt custom-safe-themes t)

;; Transparency
(set-frame-parameter nil 'alpha-background 70)

(add-to-list 'default-frame-alist '(alpha-background . 70))

;; Install theme
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-to-toggle '(modus-vivendi-deuteranopia modus-operandi-deuteranopia)
        modus-themes-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-slanted-constructs t
        modus-themes-org-blocks 'gray-background)
  (modus-themes-select 'modus-vivendi-tinted))

(use-package ef-themes
  :ensure t)

(set-face-attribute 'default nil
                    :family "RobotoMono Nerd Font"
                    :height 105
                    :weight 'medium)

(set-face-attribute 'variable-pitch nil
		    :family "Iosevka Comfy Motion Duo"
		    :height 105
		    :weight 'medium)
(set-face-attribute 'fixed-pitch nil
		    :family "Iosevka Comfy"
		    :height 105
		    :weight 'medium)

(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode))

(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?┊)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

(setq display-time-format " %a %e %b, %H:%M ")
(display-time-mode)

;; Enable battery in modeline, if on laptop
(require 'battery)
(when (and battery-status-function
           (not (string-match-p "N/A"
                                (battery-format "%B"
                                                (funcall battery-status-function)))))
  (display-battery-mode 1))



(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

(provide 'shl-appearance)
;;; shl-appearance.el ends here
