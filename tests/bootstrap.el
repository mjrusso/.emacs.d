;;; bootstrap.el --- Load dependencies for configuration tests -*- lexical-binding: t; -*-

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory load-file-name))))

(load (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)
      nil 'nomessage)
(straight-use-package '(org :type built-in))
(dolist (package '(use-package magit orgit ox-gfm ghostel))
  (straight-use-package package))
(require 'use-package)

(unless (fboundp 'set-local)
  (require 'compat-31))
