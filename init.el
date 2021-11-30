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

(defun mjr/load-all-conf-files ()
  (interactive)
  (mapc 'load (directory-files (concat user-emacs-directory "conf") t "^[^#].*el$")))

(mjr/load-all-conf-files)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
   (when (file-exists-p custom-file)
     (load custom-file))
