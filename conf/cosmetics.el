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

(when window-system
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Tweak the size and placement of the initial frame.
(when window-system
  (let* ((width (floor (/ (x-display-pixel-width) 2)))
         (height (floor (/ (x-display-pixel-height) 1)))
         (x (- (x-display-pixel-width) width (floor (* width 0.02))))
         (y (- (x-display-pixel-height) height)))
    (set-frame-position (selected-frame) x y)
    (set-frame-size (selected-frame) width height t)))

;; Tweak window split thresholds.
(setq split-width-threshold 120)
(setq split-height-threshold 160)

;; Enable the menu bar.
(menu-bar-mode t)

;; Disable the default minibuffer message.
(defun display-startup-echo-area-message ()
  (message ""))

;; Don't use native fullscreen on MacOS. (This make `toggle-frame-fullscreen` usable.)
(setq ns-use-native-fullscreen nil)

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

;; - https://github.com/atomontage/xterm-color
(use-package xterm-color)

;; Set default font to Source Code Pro.
;; - https://github.com/adobe/Source-Code-Pro
;; - http://blogs.adobe.com/typblography/2012/09/source-code-pro.html
(add-to-list 'default-frame-alist '(font . "Source Code Pro Medium"))

;; - https://github.com/tumashu/posframe
(use-package posframe)

;; - https://github.com/purcell/page-break-lines
(use-package page-break-lines)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]"
                        " [No match]" " [Matched]" " [Not readable]"
                        " [Too big]" " [Confirm]"))

(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(defun jf-ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))

(add-hook 'ido-setup-hook 'jf-ido-define-keys)
