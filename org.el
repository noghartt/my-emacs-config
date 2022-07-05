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

(setq org-directory "~/org-backup"
      org-roam-directory org-directory
      org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-capture-templates
      '(("i" "Remember" entry (file+headline "inbox.org"
                                             "Tasks")
         "** TODO %?\n"
         :unarrowed t)
        ("n" "Notebook" entry (file+headline "inbox.org"
                                             "Notebook")
         "** %^{Notebook title}\n%?"
         :unarrowed t)))

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

(provide 'org)
;;; org.el ends here
