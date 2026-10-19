;;; sync-packages.el --- Apply package pins and rebuild -*- lexical-binding: t; -*-

(setq user-emacs-directory
      (file-name-as-directory
       (expand-file-name ".." (file-name-directory load-file-name))))

(load (expand-file-name "scripts/package-versions.el" user-emacs-directory) nil t)
(let ((count (my/sync-package-versions)))
  (setq use-package-expand-minimally t)
  (load (expand-file-name "init.el" user-emacs-directory) nil t)
  (straight-check-all)
  (when (my/package-version-differences)
    (error "Package checkouts still differ from the lockfile"))
  (load (expand-file-name "scripts/sync-ghostel-module.el" user-emacs-directory) nil t)
  (my/sync-ghostel-module (ghostel--module-directory))
  (message "Synced %d repositories and checked package builds and Ghostel's module. Restart Emacs."
           count))
