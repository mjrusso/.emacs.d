(global-set-key (kbd "C-x C-b") 'ibuffer)

;; HippieExpand looks at the word before point and tries to expand it in
;; various ways including expanding from a fixed list (like `‘expand-abbrev’’),
;; expanding from matching text found in a buffer (like `‘dabbrev-expand’’) or
;; expanding in ways defined by your own functions.
;;
;; https://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

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
