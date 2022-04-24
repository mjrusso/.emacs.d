(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c g") 'magit-status)

;; Build, run, and test, using Projectile.
;; Particularly handy in conjunction with .dir-locals.el; see:
;; - https://emacs.stackexchange.com/a/706
;; - https://emacs.stackexchange.com/a/15290
;; - https://docs.projectile.mx/projectile/projects.html#storing-project-settings
(global-set-key (kbd "C-c C-b") 'projectile-compile-project)
(global-set-key (kbd "C-c C-r") 'projectile-run-project)
(global-set-key (kbd "C-c C-t") 'projectile-test-project)
(global-set-key (kbd "C-c C-c") 'projectile-repeat-last-command)
