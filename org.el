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

(provide 'org)
;;; org.el ends here