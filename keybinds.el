;;; keybinds.el  -*- lexical-binding: t; -*-

;;; Code:
(defconst leader-key "SPC"
  "The leader prefix key for Evil.")

(defconst localleader-key (concat leader-key " m")
  "The localleader prefix key, for major-mode specific commands.")

;;;; Installing general:
(use-package general)

;;;; Installing which-key:
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(provide 'keybinds)
;;; keybinds.el ends here
