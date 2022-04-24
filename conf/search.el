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

