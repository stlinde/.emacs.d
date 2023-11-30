;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; Theme
(use-package catppuccin-theme
  :ensure t
  :init
  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm))

;; Disable unwanted ui
(push '((menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(horizontal-scroll-bars)
	(vertical-scroll-bars))
      default-frame-alist)

;; Some layout options
(push '((min-height . 1)
	(height	   . 45)
	(min-width  . 1)
	(width	   . 81)
	(internal-border-width . 8)
	(left-fringe    . 1)
	(right-fringe   . 1))
      default-frame-alist)

;; Padding for windows
(use-package spacious-padding
  :ensure t
  :init
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 2
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))
  (spacious-padding-mode))

;; Set font
(push '(font . "JetBrains Mono-10") default-frame-alist)

;; Modeline
(setq modeline-compact nil)
(display-time-mode 1)
(display-battery-mode 1)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode)
  :custom
  (doom-modeline-hud t)
  (doom-modeline-buffer-encoding nil))

(provide 'shl-appearance)
;; shl-appearance.el ends here
