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

;; https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode

  :disabled

  :init

  ;; From solaire-mode:
  ;;
  ;; > Emacs will always display one of *Minibuf-N* or *Echo Area N* (where X
  ;; > is 0 or 1) in the minibuffer area. If these buffers don't exist OR they
  ;; > exist and are empty, they will be transparent, showing the (incorrect)
  ;; > background color of `default', but we want it to display
  ;; > `solaire-default-face' instead, so we create these buffers early and
  ;; > insert whitespace in them.
  ;;
  ;; Hack: ensure that this trailing whitespace is not highlighted.

  (defun my/solaire-mode-minibuf-echo-area-hide-trailing-whitespace ()
    (dolist (buf '(" *Minibuf-0*" " *Minibuf-1*"
                   " *Echo Area 0*" " *Echo Area 1*"))
      (with-current-buffer (get-buffer buf)
        (setq-local show-trailing-whitespace nil))))

  (defvar my/after-solaire-mode-fix-minibuffer-hook nil
    "Hook run after `solaire-mode-fix-minibuffer'.")

  (defadvice solaire-mode-fix-minibuffer (after run-after-solaire-mode-fix-minibuffer-hook activate)
    "Run `my/after-solaire-mode-fix-minibuffer-hook'."
    (run-hooks 'my/after-solaire-mode-fix-minibuffer-hook))

  (add-hook 'my/after-solaire-mode-fix-minibuffer-hook #'my/solaire-mode-minibuf-echo-area-hide-trailing-whitespace)
  (add-hook 'minibuffer-setup-hook #'my/solaire-mode-minibuf-echo-area-hide-trailing-whitespace)
  (add-hook 'window-configuration-change-hook #'my/solaire-mode-minibuf-echo-area-hide-trailing-whitespace)

  (solaire-global-mode +1))
