;;; magit.el -*- lexical-binding: t; -*-

;;; Code:
(use-package magit)

; NOTE: I don't know if this is the best way to handle with it, but on `evil-collection',
;       the SPC keybind is mapped to run `magit-diff-show-or-scroll-up', but it's
;       overring the SPC from evil-mode. With this unbind, we are fixing it.
(general-unbind 'magit-mode-map
  "SPC" nil)

(defvar my/global-magit-map (make-sparse-keymap))
(general-define-key
 :keymaps 'my/global-magit-map
 "g" #'magit-status)

(leader-def
 :states 'normal
 "g" my/global-magit-map)

(provide 'magit)
;;; magit.el ends here
