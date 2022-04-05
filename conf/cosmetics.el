(when window-system
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

;; Set default font to Source Code Pro.
;; - https://github.com/adobe/Source-Code-Pro
;; - http://blogs.adobe.com/typblography/2012/09/source-code-pro.html
(add-to-list 'default-frame-alist '(font . "Source Code Pro Medium"))

;; Display emojis in buffer, when Apple's emoji font is available.
(when (member "Apple Color Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; Search and insert emoji by name.
;; - https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :config
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  :bind ("C-c i e" . emojify-insert-emoji))

;; - https://github.com/tumashu/posframe
(use-package posframe)

;; - https://github.com/purcell/page-break-lines
(use-package page-break-lines)
