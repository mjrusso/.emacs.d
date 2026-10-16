;;; bootstrap.el --- Load dependencies for configuration tests -*- lexical-binding: t; -*-

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory load-file-name))))

(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)
      nil 'nomessage)
(straight-use-package '(org :type built-in))
(dolist (package '(use-package magit orgit ox-gfm))
  (straight-use-package package))
(require 'use-package)

;; Emacs 31 requires this variable to be bound before Magit's init code runs.
(defvar magit-auto-revert-mode nil)
