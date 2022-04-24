(use-package isearch
  :straight nil
  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-r" . isearch-backward-regexp)
;; ("C-M-s" . isearch-forward)
;; ("C-M-r" . isearch-backward)
;; ("M-%" . query-replace-regexp)
;; ("C-M-%" . query-replace)
   :map isearch-mode-map
   ("C-q" . isearch-delete-char)
   ("DEL" . isearch-del-char)))

;; Note that this is only required for `projectile-ripgrep'; for running
;; ripgrep interactively, prefer `deadgrep'.
(use-package ripgrep)

;; https://github.com/Wilfred/deadgrep
(use-package deadgrep
  :commands deadgrep
  :bind
  (("s-R" . deadgrep)))

;; https://github.com/jacktasia/dumb-jump
;;
;; Also see: https://www.reddit.com/r/emacs/comments/hzxvke/comment/fzo7t3w/
(use-package dumb-jump

  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-force-searcher 'rg)

  ;; Disable tags.
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))

  ;; Use dumb-jump only if there aren't any other options (for example, if the
  ;; language mode doesn't provide its own backend function).
  ;;
  ;; Note that the alternative, as suggested in the dumb-jump README, is as
  ;; follows: `(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)'
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t)
  )
