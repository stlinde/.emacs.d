;; -*- coding: utf-8; lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-ref-find)
         ("C-c n g" . org-roam-show-graph)
         ("C-c n c" . org-roam-capture)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap ("C-c n d" . org-roam-dailies-map)
  :init
  (setq org-roam-directory (file-truename "~/hempellinde")
        org-roam-database-connector 'sqlite-builtin
        org-roam-db-gc-threshold most-positive-fixnum
        org-id-link-to-org-use-id t
        org-roam-dailies-directory "mnemosyne/")
  (setq org-roam-capture-templates
      '(("m" "memex" plain "%?"
         :if-new (file+head "memex/%<%Y%m%d%H%M%S>_${slug}.org"
                            "#+title: ${title}\n#+created: %U\n")
         :immediate-finish t
         :unnarrowed t)
        ("i" "inbox" plain "%?"
         :if-new
         (file+head "inbox/%<%Y%m%d%H%M%S>_${title}.org" "#+title: ${title}\n#+created: %U\n")
         :immediate-finish t
         :unnarrowed t)))
  (setq org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%I:%M %p>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode +1))

(provide 'shl-org-roam)
;; shl-org-roam.el ends here
