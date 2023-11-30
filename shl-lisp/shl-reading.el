;; -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; For efficiently handling pdf
(use-package pdf-tools
  :ensure t
  :init
  (pdf-loader-install))

;; For annotating documents with org
(use-package org-noter
  :ensure t)

(provide 'shl-reading)
;;; shl-reading.el ends here
