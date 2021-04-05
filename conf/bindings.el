(global-set-key (kbd "C-c g") 'magit-status)

;; S-r: search entire project
;; C-u S-r (or M-1 S-r): allows user to specify search directory
(global-set-key [(super r)] 'projectile-ripgrep)

(global-set-key [(super f)] 'projectile-find-file)
(global-set-key [(super d)] 'projectile-find-dir)

;; Search
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch)
(global-set-key (kbd "C-S-s") 'swiper-isearch-thing-at-point)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)
