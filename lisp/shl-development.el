;;; shl-development.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eat
  :ensure '(eat :type git
                :host codeberg
                :repo "akib/emacs-eat"
                :files ("*.el" ("term" "term/*.el") "*.texi"
                        "*.ti" ("terminfo/e" "terminfo/e/*")
                        ("terminfo/65" "terminfo/65/*")
                        ("integration" "integration/*")
                        (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package treesit-auto
  :ensure t
  :config
  (setopt treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package eglot
  :ensure nil
  :hook ((python-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)))

(setq tab-always-indent 'complete)

(use-package orderless
  :ensure t
  :config

  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic)))

  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  (setq completion-cycle-threshold 4))



(use-package corfu
  :ensure t
  :config
  ;; Customize Corfu
  (setq global-corfu-modes '((not erc-mode
                                  circe-mode
                                  help-mode
                                  gud-mode
                                  eat-mode
                                  inferior-python-mode)
                             t)
        corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-cycle t
        corfu-separator ?\s
        corfu-preselect 'first
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current nil
        corfu-on-exact-match nil
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator)
  (setq-default corfu-quit-no-match 'separator)
  (setq text-mode-ispell-word-completion nil)

  (global-corfu-mode 1)

  ;; Rebinding keys
  (with-eval-after-load 'corfu
    (define-key corfu-map (kbd "C-y") 'corfu-insert)
    (define-key corfu-map (kbd "RET") nil))

  ;; Corfu history
  (with-eval-after-load 'corfu
    (require 'corfu-history)
    (add-hook 'corfu-mode-hook 'corfu-history-mode)

    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'corfu-history)))

  ;; Popopinfo
  (with-eval-after-load 'corfu
    (require 'corfu-popupinfo)
    (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
    (setq corfu-popupinfo-delay '(0.5 . 1.0))))


(use-package envrc
  :ensure t
  :config
  (with-eval-after-load 'envrc
    (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))
  (add-hook 'after-init-hook #'envrc-global-mode))

(use-package cape
  :ensure t
  :config
  (add-hook 'prog-mode-hook (defun shl/corfu-add-cape-file-h ()
                              (add-hook 'completion-at-point-function #'cape-file -10 t)))
  (add-hook 'org-mode-hook (defun shl/corfu-add-cape-elisp-block-h ()
                             (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))))

(provide 'shl-development)
;;; shl-development.el ends here
