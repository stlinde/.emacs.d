;;; shl-c.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'cc-mode)
  ;; Custom style based on linux
  (setq c-basic-offset tab-width
        c-backspace-function #'delete-backward-char)


  (c-add-style
   "doom" '((c-comment-only-line-offset . 0)
            (c-basic-offset . 8)
            (c-hanging-braces-alist (brace-list-open)
                                    (defun-open)
                                    (brace-entry-open)
                                    (substatement-open after)
                                    (block-close . c-snug-do-while)
                                    (arglist-cont-nonempty))
            (c-cleanup-list brace-else-brace)
            (c-offsets-alist
             (knr-argdecl-intro . 0)
             (substatement-open . 0)
             (substatement-label . 0)
             (statement-cont . +)
             (case-label . +)
             ;; align args with open brace OR don't indent at all (if open
             ;; brace is at eolp and close brace is after arg with no trailing
             ;; comma)
             (brace-list-intro . 0)
             (brace-list-close . -)
             (arglist-intro . +)
             ;; (arglist-close +cc-lineup-arglist-close 0)
             ;; don't over-indent lambda blocks
             (inline-open . 0)
             (inlambda . 0)
             ;; indent access keywords +1 level, and properties beneath them
             ;; another level
             (access-label . -)
             ;; (inclass +cc-c++-lineup-inclass +)
             (label . 0))))

  (when (listp c-default-style)
    (setf (alist-get 'other c-default-style) "doom")))

(maybe-require-package 'cmake-mode)

(provide 'shl-c)
;;; shl-c.el ends here
