;;; init.el -*- lexical-binding: t; -*-

;;; Code:
(defun display-startup-time ()
  "A simple function that display in how much seconds this config has been loaded."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time)

;;;;; Installing straight.el:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;; Load some internal files:
(load-file "keybinds.el")
(load-file "org.el")
(load-file "magit.el")
(require 'keybinds)
(require 'org)
(require 'magit)

;;;; Installing vertico:
(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

;;;; Installing marginalia:
(use-package marginalia
  :init
  (marginalia-mode))

;;;; Installing company:
(use-package company
  :config
  (add-hook 'company-mode-hook #'evil-normalize-keymaps)
  (setq company-global-modes
        '(not help-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend)))

(global-company-mode)

(general-define-key
 :keymaps 'company-mode-map
 "C-SPC" #'company-complete-common)
(general-define-key
 :keymaps 'company-active-map
 [escape] #'company-abort)

;;;; Installing smartparens:
(use-package smartparens
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode))

;;;; Installing themes:
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-plain t)
  (doom-themes-org-config))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;;;; Installing tree-sitter:
(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

;;;; Other things:
(setq-default fill-column 80)

(setq visible-bell nil)
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      auto-window-vscroll nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

; Add highlight on cursor line
(global-hl-line-mode 1)

; Add 80 columns rule indicator
(setq-default display-fill-column-indicator 79)
(global-display-fill-column-indicator-mode 1)

; Add line number column
(global-display-line-numbers-mode 1)

;;;; Reduce GC threshold:
(setq gc-cons-threshold (* 2 1000 1000))
;;;; Improve performance:
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      create-lockfiles nil)

;;; init.el ends here
