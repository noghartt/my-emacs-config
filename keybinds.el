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

;;;; Installing evil:
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (evil-mode)
  :config
  (setq evil-default-cursor nil))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

;;;; Setup keybinds:

;;;;; Evil keybinds:
(general-create-definer leader-def
  :prefix leader-key)

(general-create-definer localleader-def
  :prefix localleader-key)

(leader-def
 :states 'normal
 :keymaps 'override
 ":" #'execute-extended-command
 "h" '(:keymap help-map :which-key "help")
 "w" '(:keymap evil-window-map :which-key "window")
 "b" '(:which-key "buffer"))

(leader-def
 :states 'normal
 :keymaps 'override
 :prefix (concat leader-key " b")
 "i" #'ibuffer)

;;;; Installing helpful:
(use-package helpful
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  (general-define-key [remap describe-variable] #'helpful-variable
                      [remap describe-function] #'helpful-function))

(provide 'keybinds)
;;; keybinds.el ends here
