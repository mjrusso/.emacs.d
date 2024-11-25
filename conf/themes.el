;; https://github.com/protesilaos/modus-themes
(use-package modus-themes
  :config
  ;; Remove the border around the mode line, for all Modus themes.
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)))
  )

;; https://github.com/protesilaos/ef-themes
(use-package ef-themes)

;; https://github.com/doomemacs/themes
;;
;; Note that I'm seeing this same issue in the terminal (despite *not* having
;; solaire-mode enabled):
;; https://discourse.doomemacs.org/t/different-bg-color-between-emacsclient-nw-and-emacs-nw/160
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; (doom-themes-org-config)
  )

;; https://github.com/protesilaos/modus-themes
(load-theme 'modus-vivendi t)

;; Synchronize the theme with system appearance changes. See:
;; https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun my/apply-theme-after-system-appearance-change (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (when (window-system)
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi t))
      ('dark (load-theme 'modus-vivendi t)))))


;; (defun on-after-init ()
;;   (unless (display-graphic-p (selected-frame))
;;     (set-face-background 'default "unspecified-bg" (selected-frame))))
;;
;; (add-hook 'window-setup-hook 'on-after-init)


(add-hook 'ns-system-appearance-change-functions #'my/apply-theme-after-system-appearance-change)

;; https://www.reddit.com/r/emacs/comments/4v7tcj/comment/d5wyu1r/
(defvar my/after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `my/after-load-theme-hook'."
  (run-hooks 'my/after-load-theme-hook))
