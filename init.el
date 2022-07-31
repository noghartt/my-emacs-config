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
(use-package keybinds
  :straight nil
  :load-path "site-lisp/keybinds")
(use-package org
  :straight nil
  :load-path "site-lisp/org")
(use-package magit
  :straight nil
  :load-path "site-lisp/magit")

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

;;;; Installing projectile:
(use-package projectile
  :init
  (projectile-mode t))

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

; Add undo-tree
(use-package undo-tree
  :init
  (undo-tree-mode))

;;;; Installing typescript-mode:
(use-package typescript-mode
  :after tree-sitter
  :config
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;;;; Improve performance:
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      create-lockfiles nil)

;;; init.el ends here
