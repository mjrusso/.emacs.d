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
