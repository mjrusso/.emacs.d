(use-package isearch
  :straight nil
  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-r" . isearch-backward-regex)
;; ("C-M-s" . isearch-forward)
;; ("C-M-r" . isearch-backward)
;; ("M-%" . query-replace-regexp)
;; ("C-M-%" . query-replace)
   :map isearch-mode-map
   ("C-q" . isearch-delete-char)
   ("DEL" . isearch-del-char)))

(use-package ripgrep)
