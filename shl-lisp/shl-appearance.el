;; -*- coding: utf-8; lexical-binding: t -*-

;;; Commentary:
;;; Code:

;; Theme
(use-package modus-themes
  :ensure t
  :init
  (setq modus-themes-custom-auto-reload nil
        modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui nil
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-org-blocks nil
        modus-themes-completions '((t . (extrabold)))
        modus-themes-prompts '(extrabold)
        modus-themes-headings
        '((agenda-structure . (variable-pitch light 2.2))
          (agenda-date . (variable-pitch regular 1.3))
          (t . (regular 1.15))))
  ;; To list the palette's colours, use `modus-themes-list-colors',
  ;; `modus-themes-list-colors-current'.  With a prefix argument
  ;; (`C-u'), they show only the semantic colour mappings, meaning
  ;; those which reference a named colour from the palette, but which
  ;; do not define a new colour themselves.
  (setq modus-themes-common-palette-overrides
        `((cursor cyan-intense)
          (bg-region bg-ochre)
          (fg-region unspecified)
          (bg-paren-match bg-blue-intense)
          (bg-mode-line-active bg-lavender)
          (border-mode-line-active magenta-cooler)
          (border-mode-line-inactive border)
          (bg-hl-line bg-dim)
          (bg-line-number-active bg-hl-line)
          (bg-line-number-inactive unspecified)
          (fg-line-number-active fg-main)
          (bg-prompt bg-blue-nuanced)
          (fg-prompt blue-warmer)
          ;; ,@modus-themes-preset-overrides-warmer
          ))
  (load-theme 'modus-vivendi-tinted t))

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
  (spacious-padding-mode))

;; Set font
(push '(font . "JetBrainsMono Nerd Font-10") default-frame-alist)

;; Modeline
(setq modeline-compact nil)
(display-time-mode 1)
(display-battery-mode 1)

(provide 'shl-appearance)
;; shl-appearance.el ends here
