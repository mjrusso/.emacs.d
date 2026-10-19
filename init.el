;; -*- lexical-binding: nil; -*-

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

(load (expand-file-name "scripts/package-versions.el" user-emacs-directory) nil t)
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-idle-timer 3 nil #'my/warn-package-version-differences)))

(defun my/load-all-conf-files (directory-name)
  (interactive)
  (mapc 'load (directory-files (concat user-emacs-directory directory-name) t "^[^#].*el$")))

(defun my/load-conf-file (conf-file-name)
  (load (concat user-emacs-directory
                (file-name-as-directory "conf")
                conf-file-name)))

(my/load-conf-file "startup.el")
(my/load-conf-file "path.el")
(my/load-conf-file "core.el")
(my/load-conf-file "themes.el")
(my/load-conf-file "frames.el")
(my/load-conf-file "isolation.el")
(my/load-conf-file "tramp.el")
(my/load-conf-file "completion.el")
(my/load-conf-file "cosmetics.el")
(my/load-conf-file "fonts.el")
(my/load-conf-file "fun.el")
(my/load-conf-file "graphics.el")
(my/load-conf-file "minibuffer.el")
(my/load-conf-file "misc.el")
(my/load-conf-file "mode-line.el")
(my/load-conf-file "mouse.el")
(my/load-conf-file "nav.el")
(my/load-conf-file "notes.el")
(my/load-conf-file "presentations.el")
(my/load-conf-file "programming.el")
(my/load-conf-file "code-review.el")
(my/load-conf-file "project.el")
(my/load-conf-file "scratch.el")
(my/load-conf-file "screenwriting.el")
(my/load-conf-file "search.el")
(my/load-conf-file "snippets.el")
(my/load-conf-file "spelling.el")
(my/load-conf-file "tabs.el")
(my/load-conf-file "vterm.el")
(my/load-conf-file "ghostel.el")
(my/load-conf-file "terminal.el")
(my/load-conf-file "unicode.el")
(my/load-conf-file "windows.el")
(my/load-conf-file "workspaces.el")
(my/load-conf-file "writing.el")
(my/load-conf-file "ai/gptel.el")
(my/load-conf-file "ai/autocomplete.el")

;; If any private configuration files exist, load them. (Private configuration
;; files aren't committed to this repository.)
(let* ((directory-name "conf-private")
       (private-conf-directory (concat user-emacs-directory directory-name)))
  (if (file-directory-p private-conf-directory)
      (my/load-all-conf-files directory-name)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'server)

;; Something else may start a server later (`with-editor' does, for commit
;; messages); make sure it can't take `server', where `emacsclient' looks.
(unless (daemonp)
  (setq server-name (format "server%s" (emacs-pid))))

;; Let the daemon own the state files in `user-emacs-directory': a second Emacs
;; sharing the directory would write back the stale snapshot it loaded at
;; startup. `savehist' saves on a timer, so dropping the hooks isn't enough.
(unless (daemonp)
  (setq transient-save-history nil
        bookmark-save-flag nil)
  (remove-hook 'kill-emacs-hook #'savehist-autosave)
  (remove-hook 'kill-emacs-hook #'save-place-kill-emacs-hook)
  (remove-hook 'kill-emacs-hook #'recentf-save-list)
  (when (timerp (bound-and-true-p savehist-timer))
    (cancel-timer savehist-timer)
    (setq savehist-timer nil)))
