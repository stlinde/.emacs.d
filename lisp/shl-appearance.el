;;; shl-appearance.el --- -*- lexical-binding: t -*-
;;; Code:
;;; Commentary:

;; Don't ask if theme is safe
(setopt custom-safe-themes t)

(defvar shl--theme 'modus-operandi-deuteranopia)
(defvar shl--font-weight 'medium)

;; Padding
(use-package spacious-padding
  :ensure t
  :defer nil
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))
  (setq spacious-padding-subtle-mode-line
      `( :mode-line-active 'default
         :mode-line-inactive vertical-border))
  (spacious-padding-mode 1))

;; Transparency
(set-frame-parameter nil 'alpha-background 100)
(add-to-list 'default-frame-alist '(alpha-background . 100))

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
        modus-themes-org-blocks 'gray-background))
  ;; (modus-themes-select shl--theme))

(use-package ef-themes
  :ensure t
  :defer nil
  :init
  (defun shl--ef-themes-mode-line ()
  "Tweak the style of the mode lines."
  (ef-themes-with-colors
    (custom-set-faces
     `(mode-line ((,c :background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,fg-dim))))
     `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))

  (add-hook 'ef-themes-post-load-hook  #'shl--ef-themes-mode-line)
  :config
  (setopt ef-themes-variable-pitch-ui t
          ef-themes-mixed-fonts t)
  (ef-themes-select 'ef-dream))

(set-face-attribute 'default nil
                    :family "Iosevka Comfy"
                    :height 105
                    :weight shl--font-weight)
(set-face-attribute 'variable-pitch nil
		    :family "Iosevka Comfy Motion Duo"
		    :height 105
		    :weight shl--font-weight)
(set-face-attribute 'fixed-pitch nil
		    :family "Iosevka Comfy"
		    :height 105
		    :weight shl--font-weight)

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
