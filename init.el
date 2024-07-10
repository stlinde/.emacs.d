;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst my-emacs-d (file-name-as-directory user-emacs-directory)
  "Directory of emacs.d.")

(defconst my-site-lisp-dir (concat my-emacs-d "site-lisp")
  "Directory of site-lisp.")

(defconst my-lisp-dir (concat my-emacs-d "lisp")
  "Directory of personal configuration.")

(defun require-init (pkg &optional maybe-disabled)
  "Load PKG if MAYBE-DISABLED is nil or it's nil but start up in normal slowly."
  (when (or (not maybe-disabled) (not my-lightweight-mode-p))
    (load (file-truename (format "%s/%s" my-lisp-dir pkg)) t t)))

(let* ((file-name-handler-alist nil))
  (require-init 'shl-elpaca)
  (require-init 'shl-startup)
  (require-init 'shl-appearance)
  (require-init 'shl-defaults)
  (require-init 'shl-minibuffer)
  (require-init 'shl-editing)
  (require-init 'shl-development)
  (require-init 'shl-vc)
  (require-init 'shl-python)
  (require-init 'shl-org)
  (require-init 'shl-misc)
  )

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
