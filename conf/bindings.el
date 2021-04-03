(global-set-key (kbd "C-c g") 'magit-status)

;; S-r: search entire project
;; C-u S-r (or M-1 S-r): allows user to specify search directory
(global-set-key [(super r)] 'projectile-ripgrep)

(global-set-key [(super f)] 'projectile-find-file)
(global-set-key [(super d)] 'projectile-find-dir)
