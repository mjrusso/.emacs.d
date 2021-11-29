;; Prevent package.el from automatically loading packages at startup. See:
;; https://github.com/raxod502/straight.el#getting-started
(setq package-enable-at-startup nil)

;; Turn off the mouse interface early in startup to avoid momentary display.
(progn
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode horizontal-scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))
