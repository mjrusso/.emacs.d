(dolist (theme '(darkokai-theme
                 monokai-theme
                 zenburn-theme
                 twilight-theme
                 solarized-theme
                 espresso-theme
                 tango-plus-theme))
  (straight-use-package theme))

(use-package darkokai-theme
  :config (load-theme 'darkokai t))
;; https://github.com/doomemacs/themes
(use-package doom-themes

  :config

  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

;;  (load-theme 'doom-vibrant t)
  (load-theme 'doom-one-light t)

  (doom-themes-org-config)
  )

;; https://www.reddit.com/r/emacs/comments/4v7tcj/comment/d5wyu1r/
(defvar my/after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `my/after-load-theme-hook'."
  (run-hooks 'my/after-load-theme-hook))

