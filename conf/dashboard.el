;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :after (counsel projectile page-break-lines)
  :init
  (setq dashboard-set-init-info t)
  (setq dashboard-startup-banner 3)
  (setq dashboard-center-content t)
  (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-projects-switch-function 'my/open-project-in-new-perspective-action)
  (setq dashboard-items '(
                          (bookmarks . 12)
                          (recents  . 12)
                          (projects . 12)
                          ;; (agenda . 5)
                          ;; (registers . 5)
                          ))
  (setq dashboard-footer-messages '("Welcome to the church of Emacs"
                                    "While any text editor can save your files, only Emacs can save your soul"))
  :config
  (dashboard-setup-startup-hook))


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
