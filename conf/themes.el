;; https://github.com/protesilaos/modus-themes
(use-package modus-themes
  :config
  ;; Restore bold rendering for faces like `error', `warning', `success',
  ;; `magit-diff-file-heading-highlight', etc. In modus-themes 5.0.0+ these
  ;; inherit from `modus-themes-bold', which is only bold when this is non-nil.
  (setq modus-themes-bold-constructs t)
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

;; https://github.com/tinted-theming/base16-emacs
(use-package base16-theme
  :config
  (setq base16-theme-256-color-source 'terminal))

(defun my/use-terminal-colors ()
  "Disable all active themes to inherit colors from the terminal."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (message "Using terminal-defined colors."))

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

(defun my/apply-theme-appearance (appearance)
  "Load the default theme for APPEARANCE.
APPEARANCE must be either `light' or `dark'."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (my/light-theme))
    ('dark (my/dark-theme))))

(defun my/theme-appearance-from-env ()
  "Return `light' or `dark' from SYSTEM_APPEARANCE, or nil if unset.
Set SYSTEM_APPEARANCE=light or SYSTEM_APPEARANCE=dark in the
environment to pass system appearance through terminal sessions
and SSH."
  (let ((appearance (getenv "SYSTEM_APPEARANCE")))
    (when appearance
      (pcase (downcase (string-trim appearance))
        ("light" 'light)
        ("dark" 'dark)))))

(defun my/system-theme-appearance ()
  "Return the preferred theme appearance, either `light' or `dark'."
  (or (my/theme-appearance-from-env)
      (when (eq system-type 'darwin)
        (if (system-dark-mode-enabled-p) 'dark 'light))
      'dark))

;; Synchronize the theme with system appearance changes. See:
;; https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun my/apply-theme-after-system-appearance-change (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (when (display-graphic-p)
    (message "System appearance changed to %s" appearance)
    (my/apply-theme-appearance appearance)))

(defun system-dark-mode-enabled-p ()
  "Return non-nil if system dark mode is enabled on macOS."
  (when (eq system-type 'darwin)
    (string-equal
     "Dark"
     (string-trim
      (shell-command-to-string
       "defaults read -g AppleInterfaceStyle 2>/dev/null")))))

;; Prefer explicit terminal/SSH environment, then macOS appearance, then dark.
(let ((appearance (my/system-theme-appearance)))
  (my/apply-theme-appearance appearance)
  (message "Using %s theme" appearance))

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
