(defvar my/personal-snippets-dir (file-truename "~/Dropbox/snippets/")
  "Location of my personal snippets")

;; https://github.com/joaotavora/yasnippet
;;
;; Docs: https://joaotavora.github.io/yasnippet/
(use-package yasnippet
  :defer 10
  :custom
  (yas-prompt-functions '(yas-completing-prompt))
  (yas-snippet-dirs '(my/personal-snippets-dir))
  :init
  (yas-global-mode 1))

;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :after yasnippet)

;; https://github.com/mohkale/consult-yasnippet
(use-package consult-yasnippet
  :after (yasnippet consult)
  :bind (("s-s" . consult-yasnippet)))
