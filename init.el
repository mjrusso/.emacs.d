;; Bootstrap straight.el, and configure use-package accordingly.

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

(use-package straight
             :custom (straight-use-package-by-default t))

(defvar my-packages '(magit
                      company
                      lsp-mode
                      lsp-ui
                      lsp-treemacs
                      lsp-sourcekit
                      swift-mode
                      org-roam
                      org-download
                      ox-reveal
                      add-node-modules-path
                      clojure-mode
                      counsel-projectile
                      deft
                      exec-path-from-shell
                      flx
                      flx-ido
                      fountain-mode
                      highlight-indentation
                      htmlize
                      idle-highlight-mode
                      ido-completing-read-plus
                      imenu-list
                      markdown-mode
                      olivetti
                      paredit
                      prettier-js
                      projectile
                      ripgrep
                      rust-mode
                      smex
                      swiper
                      typo
                      web-mode
                      xterm-color))

(dolist (p my-packages)
  (straight-use-package p))

(defun mjr/load-all-conf-files ()
  (interactive)
  (mapc 'load (directory-files (concat user-emacs-directory "conf") t "^[^#].*el$")))

(mjr/load-all-conf-files)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
   (when (file-exists-p custom-file)
     (load custom-file))
