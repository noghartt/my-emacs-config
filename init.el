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

;;;; Initialize package sources:
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa")
                         ("elpa"  . "https://elpa.gnu.org/packages")))

;;;; Reduce GC threshold:
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
