;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :after (counsel projectile)
  :init
  (setq dashboard-set-init-info t)
  (setq dashboard-startup-banner 3)
  (setq dashboard-center-content t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-projects-switch-function 'mjr/open-project-in-new-tab-action)
  (setq dashboard-items '((recents  . 5)
                          (projects . 10)
                          (bookmarks . 5)
                          ;; (agenda . 5)
                          (registers . 5)
                          ))
  (setq dashboard-footer-messages '("Welcome to the church of Emacs"
                                    "While any text editor can save your files, only Emacs can save your soul"))
  :config
  (dashboard-setup-startup-hook))
