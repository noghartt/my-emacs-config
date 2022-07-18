;;; org.el  -*- lexical-binding: t; -*-
;;
;;; Code:

;;;; Installing org-roam:
(use-package org-roam
  :config
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :target (file+head "${slug}.org"
                                                         "#+title: ${title}\n")
                                      :unarrowed t))))

(require 'org-roam-protocol)

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after 'org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

;;;; Installing org-ref:
(use-package org-ref
  :after 'org-roam
  :config
  (setq org-ref-insert-cite-function
        (lambda ()
          (org-cite-insert nil))))

;;;; Prettify org-mode:
(use-package org-superstar
  :after org-mode
  :hook
  (org-mode . org-supertar-mode))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode))

(add-to-list 'org-mode-hook (lambda () (org-indent-mode t)))

(setq org-modules '(org-habit
                    org-protocol))

(eval-after-load 'org
  '(org-load-modules-maybe t))

(setq org-directory "~/org-backup"
      org-default-notes-file (concat org-directory "/inbox.org")
      org-roam-directory (concat org-directory "/notes")
      org-roam-dailies-directory (concat org-directory "/dailies"))

(setq org-capture-templates
      '(("i" "Inbox")
	("it" "Tasks")
        ("itt" "Just a simple tasks" entry
	 (file+headline "inbox.org"
                        "Tasks")
         "* TODO %?\n"
         :prepend t
         :unarrowed t
	 :refile-targets (("inbox.org" :maxlevel . 6)))
	("its" "Scheduled task" entry
	 (file+headline "inbox.org"
			"Tasks")
	 "* TODO %^{Task title}\nSCHEDULED: %^{Date}T\n%?")
        ("in" "Notebook" entry (file+headline "inbox.org"
                                             "Notebook")
         "* %^{Notebook title}\n%?"
         :unarrowed t)
	("il" "For later" entry
	 (file+headline "inbox.org"
			"For later")
	 :unarrowed t)))

;; TODO: Add roam dailies directory as agenda files
(setq org-agenda-files `(,(concat org-directory "/inbox.org")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")))

(setq org-refile-targets '((nil :maxlevel . 3)
			   (org-agenda-files :maxlevel . 3))
      org-refile-allow-creating-parent-nodes t
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

;;;; Set keybinds:
(defvar my/global-org-roam-mode-map (make-sparse-keymap))
(general-define-key
 :keymaps 'my/global-org-roam-mode-map
 "n" #'org-roam-capture
 "t" #'org-roam-buffer-toggle
 "f" #'org-roam-node-find)

(defvar my/global-org-mode-map (make-sparse-keymap))
(general-define-key
 :keymaps 'my/global-org-mode-map
 "n" #'org-capture
 "r" my/global-org-roam-mode-map) ; TODO: Add `:wk' in this map as "roam"

(leader-def
 :states 'normal
 "n" my/global-org-mode-map)

;;;; Installing elfeed:
(use-package elfeed
  :config
  (setq-default elfeed-search-filter "@1-week-ago +unread"))

(use-package elfeed-org
  :after elfeed
  :init
  (elfeed-org))

(use-package elfeed-goodies
  :after elfeed
  :init
  (elfeed-goodies/setup))

(setq rmh-elfeed-org-files '((concat org-directory "/elfeed.org")))

;;;; Installing utils:
(use-package consult)

(localleader-def
 :states 'normal
 :keymaps 'org-mode-map
 "h" #'consult-org-heading)

(provide 'org)
;;; org.el ends here
