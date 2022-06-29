;;; keybinds.el  -*- lexical-binding: t; -*-

;;; Code:

;;;; Installing which-key:
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;;;; Installing general:
(use-package general)

;;;; Installing evil:
(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (evil-mode)
  :config
  (setq evil-default-cursor 'box
        evil-insert-state-cursor 'box))

;;;; Keybinds:
(defconst leader-key "SPC"
  "The leader prefix key for Evil.")
(defconst localleader-key "SPC m"
  "The localleader prefix key, for major-mode specific commands.")

(general-create-definer leader-def
  :prefix leader-key)
(general-create-definer localleader-def
  :prefix localleader-key)

;;;;; Global keybinds:

; TODO: I think that could be a way to add `:which-key' keyword symbol.
;       It would be cool, because we don't need to replicate `:wk' every
;       time that we're using this keymap.
(defvar my/global-files-map (make-sparse-keymap))
(general-define-key
 :keymaps 'my/global-files-map
 "s" #'save-buffer
 "f" #'find-file)

(leader-def
  :states 'normal
  "h" '(:keymap help-map :wk "help")
  "w" '(:keymap evil-window-map :wk "window")
  "f" '(:keymap my/global-files-map :wk "file")
  "m" '(:ignore t :wk "<localleader>"))

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
