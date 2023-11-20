;; -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar shl/init-file-loaded-p nil
  "Non-nil if the init-file has already been loaded.")


(cond ((and (not after-init-time) shl/init-file-loaded-p))
      (t (setq shl/init-file-loaded-p t)

	 ;; Prevent package.el from modifying this file.
	 (setq package-enable-at-startup nil)

	 ;; Prevent custom from modifying this file
	 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
	 (load custom-file 'noerror 'nomessage)

	 (defconst shl/lisp-dir (concat user-emacs-directory "shl-lisp")
	   "Directory with personal configuration.")

	 (defun require-init (pkg &optional maybe-disabled)
	   "Load PKG if MAYBE-DISABLED is nil."
	   (when (not maybe-disabled)
	     (load (file-truename (format "%s/%s" shl/lisp-dir pkg)) t t)))

	 ;; Load personal configuration
	 (let* ((file-name-handler-alist nil))
	   (require-init 'shl-startup)
	   (require-init 'shl-elpaca)
	   (require-init 'shl-defaults)
	   (require-init 'shl-minibuffer)
	   (require-init 'shl-appearance)
	   (require-init 'shl-vc)
	   (require-init 'shl-window)
	   (require-init 'shl-term)
       (require-init 'shl-search)
       (require-init 'shl-org)
       (require-init 'shl-org-roam)
       (require-init 'shl-lsp)
       (require-init 'shl-company)
       (require-init 'shl-treesitter)
       (require-init 'shl-zig)
       (require-init 'shl-editing))))

;; Local Variables:
;; no-native-compile: t
;; End:
