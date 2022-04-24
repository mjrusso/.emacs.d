;; https://github.com/elizagamedev/power-mode.el
(use-package power-mode
  :disabled
  :straight (power-mode :type git :host github :repo "elizagamedev/power-mode.el")
  :config
  (setq power-mode-streak-shake-threshold nil)
  )

;; OpenStreetMap viewer
;;
;; https://github.com/minad/osm
;;
;; Primary top-level commands: `osm-home' and `osm-search'
(use-package osm
  :disabled
  :custom
  (osm-server 'default)
  (osm-copyright nil)
  :init
  ;; Load Org link support
  (with-eval-after-load 'org
    (require 'osm-ol)))
