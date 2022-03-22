;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :after (projectile page-break-lines)
  :init
  (setq dashboard-set-init-info t)
  (setq dashboard-startup-banner 3)
  (setq dashboard-center-content t)
  (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-projects-switch-function 'my/open-project-by-name-in-new-perspective)
  (setq dashboard-items '(
                          (bookmarks . 12)
                          (recents  . 12)
                          (projects . 12)
                          ;; (agenda . 5)
                          ;; (registers . 5)
                          ))
  (setq dashboard-footer-messages '("Welcome to the church of Emacs"
                                    "While any text editor can save your files, only Emacs can save your soul"))
  )


(defun my/switch-to-dashboard ()
  "Switch to the dashboard buffer."
  (interactive)
  (switch-to-buffer "*dashboard*"))

(defun my/switch-to-dashboard-and-refresh ()
  "Refresh the dashboard buffer, and switch to it."
  (interactive)
  (dashboard-refresh-buffer)
  (my/switch-to-dashboard))

(global-set-key (kbd "C-c s d") #'my/switch-to-dashboard-and-refresh)

(defun my/default-window-setup ()
  "Called by emacs-startup-hook to set up my initial window configuration."
  (my/switch-to-dashboard)
  (split-window-right)
  (other-window 1)
  (find-file "~/Dropbox/org/inbox.org")
  (split-window-below)
  (my/find-persistent-scratch-file "scratch.el")
  (other-window 2)
  )

(add-hook 'emacs-startup-hook #'my/default-window-setup)
