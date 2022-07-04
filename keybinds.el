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
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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
  "g" '(:ignore t :wk "git/github")
  "m" '(:ignore t :wk "<localleader>"))

(defvar localleader-buffer-map (make-sparse-keymap))
(general-define-key
 :keymaps 'localleader-buffer-map
 :wk "buffer"
 "b" #'eval-buffer
 "f" #'eval-defun
 "e" #'eval-expression
 "s" #'eval-last-sexp
 "r" #'eval-region)

(localleader-def
 :states 'normal
 :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
 "e" '(:keymap localleader-buffer-map :wk "eval"))

;;;; Installing helpful:
(use-package helpful
  :commands helpful--read-symbol
  :hook (helpful-mode . visual-line-mode)
  :init
  (general-define-key [remap describe-variable] #'helpful-variable
                      [remap describe-function] #'helpful-function))

(provide 'keybinds)
;;; keybinds.el ends here
