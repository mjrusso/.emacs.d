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

(defun my/light-theme ()
  "Switch to default light theme (modus-operandi)."
  (interactive)
  (load-theme 'modus-operandi t))

(defun my/dark-theme ()
  "Switch to default dark theme (modus-vivendi)."
  (interactive)
  (load-theme 'modus-vivendi t))

(defun my/toggle-theme ()
  "Toggle between default light and dark themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (my/dark-theme)
    (my/light-theme)))

;; Synchronize the theme with system appearance changes. See:
;; https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun my/apply-theme-after-system-appearance-change (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (when (display-graphic-p)
    (message "System appearance changed to %s" appearance)
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi t))
      ('dark (load-theme 'modus-vivendi t)))))

(defun system-dark-mode-enabled-p ()
  "Return non-nil if system dark mode is enabled on macOS."
  (when (eq system-type 'darwin)
    (string-equal
     "Dark"
     (string-trim
      (shell-command-to-string
       "defaults read -g AppleInterfaceStyle 2>/dev/null")))))

;; If running on MacOS, load the theme based on system appearance. Otherwise,
;; default to the dark theme.
(if (eq system-type 'darwin)
    (if (system-dark-mode-enabled-p)
        (progn
          (my/dark-theme)
          (message "System dark mode enabled; using dark theme"))
      (progn
        (my/light-theme)
        (message "System light mode is enabled; using light theme")))
  (my/dark-theme))

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
