(when (display-graphic-p)
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (menu-bar-mode -1)
  (blink-cursor-mode -1))

;; Disable the default minibuffer message.
(defun display-startup-echo-area-message ()
  (message ""))

;; Don't use native fullscreen on MacOS. (This make `toggle-frame-fullscreen` usable.)
(setq ns-use-native-fullscreen nil)

;; Transparent titlebars, on MacOS: https://github.com/purcell/ns-auto-titlebar
(use-package ns-auto-titlebar
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

;; Don't display the proxy icon in the titlebar.
(setq ns-use-proxy-icon nil)

;; Show column numbers.
(column-number-mode t)

;; Show trailing whitespace.
(setq-default show-trailing-whitespace t)

;; But don't highlight trailing whitespace everywhere.
;; https://www.reddit.com/r/emacs/comments/e1vos6/any_way_to_disable_showtrailingwhitespace_in_the/f8rzn21
(dolist (hook '(special-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                minibuffer-setup-hook
                calendar-mode-hook))
  (add-hook hook
    (lambda () (setq show-trailing-whitespace nil))))

;; - https://github.com/tumashu/posframe
(use-package posframe :disabled)

;; - https://github.com/purcell/page-break-lines
(use-package page-break-lines)

 ;; https://github.com/tarsius/hl-todo
(use-package hl-todo
  :config
  (setq hl-todo-highlight-punctuation ":")
  ;; https://www.reddit.com/r/emacs/comments/f8tox6/comment/fipxb2w
  (setq hl-todo-keyword-faces
        `(("TODO"       error bold italic)
          ("FIXME"      warning bold italic)
          ("FUTURE"     font-lock-doc-face bold italic)
          ("HACK"       font-lock-constant-face bold)
          ("BUG"        error bold)
          ("XXX"        font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("IMPORTANT"  font-lock-constant-face italic)
          ("DEPRECATED" font-lock-doc-face bold)))
 :init
 (global-hl-todo-mode))
