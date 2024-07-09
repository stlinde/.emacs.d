;;; shl-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure t
  :config

  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c c") 'org-capture)
  (define-key global-map (kbd "C-c a") 'org-agenda)

  (with-eval-after-load 'org
    (define-key org-mode-map [remap goto-line] 'consult-org-heading))
  (defvar shl/org-global-prefix-map (make-sparse-keymap)
    "A keymap for global access to org helpers.")

  (define-key shl/org-global-prefix-map (kbd "j") 'org-clock-goto)
  (define-key shl/org-global-prefix-map (kbd "l") 'org-clock-in-last)
  (define-key shl/org-global-prefix-map (kbd "i") 'org-clock-in)
  (define-key shl/org-global-prefix-map (kbd "o") 'org-clock-out)
  (define-key global-map (kbd "C-c o") shl/org-global-prefix-map)

  (setq org-log-done t
        org-edit-timestamp-down-means-later t
        org-hide-emphasis-markers t
        org-catch-invisible-edits 'show
        org-export-coding-system 'utf-8
        org-fast-tag-selection-single-key 'expert
        org-html-validation-link nil
        org-export-kill-product-buffer-when-displayed t
        org-tags-column 80)

  (setq org-directory "/home/slinde/data/notes/")
  (setq org-default-notes-file (concat org-directory "index.org"))

  ;; Word wrapping is nice in org
  (add-hook 'org-mode-hook (lambda ()
                             (toggle-word-wrap)
                             (visual-line-mode)))
(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))

(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(with-eval-after-load 'org-agenda
                      (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun shl/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'shl/verify-refile-target)

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))

;; Set Agenda Files
(setq org-agenda-files `(,org-directory))

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
                      (add-hook 'org-agenda-mode-hook
                                (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))


(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

;; Views
(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'day
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                          '(lambda ()
                             (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                 (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                          '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                          '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                          '(lambda ()
                             (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                 (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                          '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                          '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                          '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                          '(lambda ()
                             (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                 (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                          '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))

(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;; Save the running clock and all clock history when exiting Emacs, load it on startup
(with-eval-after-load 'org
                      (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;;; Show the clocked-in task - if any - in the header line
(defun shl/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun shl/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'shl/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'shl/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'shl/hide-org-clock-from-header-line)

(with-eval-after-load 'org-clock
                      (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
                      (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))
(setopt org-confirm-babel-evaluate nil)
(setopt org-src-window-setup 'current-window)
(setopt org-edit-src-persistent-message nil)
(setopt org-src-fontify-natively t)
(setopt org-src-preserve-indentation t)
(setopt org-src-tab-acts-natively t)
(setopt org-edit-src-content-indentation 0)


(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((R . t)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (latex . t)
      (ledger . t)
      (octave . t)
      (plantuml . t)
      (python . t)
      (screen . nil)
      (sh . t) ;; obsolete
      (shell . t)
      (sql . t)
      (sqlite . t))))))

(use-package org-cliplink
  :ensure t)

;; Pomodoro
(use-package org-pomodoro
  :ensure t
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)))


;; Install org-modern
(use-package org-modern
  :ensure t
  :config
  ;; Settings
  (setopt org-auto-align-tags nil
	      org-tags-column 0
	      org-catch-invisible-edits 'show-and-error
	      org-special-ctrl-a/e t
	      org-insert-heading-respect-content t
	      org-hide-emphasis-markers t
	      org-pretty-entities t
	      org-ellipsis "…")

  ;; Enable org-modern
  (global-org-modern-mode))


;; Denote
(use-package denote
  :ensure t
  :hook ((dired-mode . denote-dired-mode))
  :bind (("C-c n n" . denote)
         ("C-c n c" . denote-region)
         ("C-c n N" . denote-type)
         ("C-c n d" . denote-date)
         ("C-c n z" . denote-signature) ;; Zettlekasten mnemonic
         ("C-c n s" . denote-subdirectory)
         ("C-c n t" . denote-template)
         ("C-c n i" . denote-link) ;; Insert mnemonic
         ("C-c n I" . denote-add-links)
         ("C-c n b" . denote-backlinks)
         ("C-c n f f" . denote-find-link)
         ("C-c n f b" . denote-find-backlink)
         ("C-c n r" . denote-rename-file)
         ("C-c n R" . denote-rename-file-using-front-matter)

         :map dired-mode-map
         ("C-c C-d C-i" . denote-link-dired-marked-notes)
         ("C-c C-d C-r" . denote-dired-rename-files)
         ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
         ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (setopt denote-directory (expand-file-name "~/data/notes/")
          denote-save-buffers nil
          denote-known-keywords '("emacs" "risk" "trading" "analytics" "yggdrasil" "python")
          denote-infer-keywords t
          denote-sort-keywords t
          denote-prompts '(title keywords)
          denote-excluded-directories-regexp nil
          denote-excluded-keywords-regexp nil
          denote-rename-confirmations '(rewrite-front-matter modify-file-name)
          denote-date-prompt-use-org-read-date t
          denote-date-format nil
          denote-backlinks-show-context t)

  (with-eval-after-load 'org-capture
    (setopt denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))




(provide 'shl-org)
;;; shl-org.el ends here
