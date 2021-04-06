(global-set-key (kbd "C-x C-b") 'ibuffer)

;; HippieExpand looks at the word before point and tries to expand it in
;; various ways including expanding from a fixed list (like `‘expand-abbrev’’),
;; expanding from matching text found in a buffer (like `‘dabbrev-expand’’) or
;; expanding in ways defined by your own functions.
;;
;; https://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-c g") 'magit-status)

;; S-r: search entire project
;; C-u S-r (or M-1 S-r): allows user to specify search directory
(global-set-key [(super r)] 'projectile-ripgrep)

(global-set-key [(super f)] 'projectile-find-file)
(global-set-key [(super d)] 'projectile-find-dir)

(global-set-key [(super p)] 'mjr/open-project)

;; Search
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-s") 'swiper-isearch)
;; (global-set-key (kbd "C-r") 'swiper-isearch)
(global-set-key (kbd "C-S-s") 'swiper-isearch-thing-at-point)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Move point from window to window using shift and the arrow keys.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Tab navigation
(global-set-key (kbd "s-}") 'tab-next)
(global-set-key (kbd "s-{") 'tab-previous)
(global-set-key (kbd "s--") 'tab-bar-select-tab-by-name)
(global-unset-key (kbd "s-_"))
(global-set-key (kbd "s-=") 'mjr/new-tab)
(global-set-key (kbd "s-+") 'mjr/open-project-in-new-tab)
(dotimes (i 9) ;; Bind s-1 through s-9 to tabs.
   (let ((d (+ i 1)))
     (global-set-key (kbd (format "s-%d" d))
                     `(lambda ()
                        (interactive)
                        (tab-bar-select-tab ,d)))))
(global-set-key (kbd "s-0") 'tab-close)
