;;; shl-python.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar shl/ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info" "--InteractiveShell.display_page=True")
  "Command to initialize the IPython repl.")

(use-package pyvenv
  :ensure t)

(use-package python
  :ensure nil
  :config
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; REPL
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt --no-color-info --InteractiveShell.display_page=True")

  ;; Docstrings
  (setq python-fill-docstring-style 'django)

  (add-hook 'python-mode-hook #'flymake-mode)
  (add-hook 'python-ts-mode-hook #'flymake-mode)

  (setq python-check-command "NO_COLOR=1 rye check"))

(use-package pet
  :ensure t
  :config
  (add-hook 'python-ts-hook 'pet-mode -10))

(provide 'shl-python)
;;; shl-python.el ends here
