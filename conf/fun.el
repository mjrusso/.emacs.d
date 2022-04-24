;; https://github.com/elizagamedev/power-mode.el
(use-package power-mode
  :disabled
  :straight (power-mode :type git :host github :repo "elizagamedev/power-mode.el")
  :config
  (setq power-mode-streak-shake-threshold nil)
  )
