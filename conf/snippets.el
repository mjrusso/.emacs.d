;; -*- lexical-binding: nil; -*-

(defvar my/personal-snippets-dir (file-truename "~/Dropbox/snippets/")
  "Location of my personal snippets")

;; https://github.com/joaotavora/yasnippet
;;
;; Docs: https://joaotavora.github.io/yasnippet/
(use-package yasnippet
  :demand t
  :custom
  (yas-prompt-functions '(yas-completing-prompt))
  (yas-snippet-dirs
   (if (file-directory-p my/personal-snippets-dir)
       '(my/personal-snippets-dir)
     (list (expand-file-name "snippets" user-emacs-directory)))))

;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :after yasnippet
  :config
  (yas-global-mode 1))

;; https://github.com/mohkale/consult-yasnippet
(use-package consult-yasnippet
  :after (yasnippet consult)
  :bind (("C-c s" . consult-yasnippet)))
