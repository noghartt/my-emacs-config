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

;;;; Other things:
(setq visible-bell nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

; Add 80 columns rule indicator
(setq-default display-fill-column-indicator 79)
(global-display-fill-column-indicator-mode 1)

; Add line number column
(global-display-line-numbers-mode 1)

;;;; Reduce GC threshold:
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
