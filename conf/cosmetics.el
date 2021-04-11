(when window-system
  (setq frame-title-format
        (list '(:eval (mjr/current-tab-name-with-index))))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Tweak window split thresholds.
(setq split-width-threshold 80)
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
                minibuffer-setup-hook))
  (add-hook hook
    (lambda () (setq show-trailing-whitespace nil))))

;; Set default font to Source Code Pro.
;; - https://github.com/adobe/Source-Code-Pro
;; - http://blogs.adobe.com/typblography/2012/09/source-code-pro.html
(add-to-list 'default-frame-alist '(font . "Source Code Pro Medium"))

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
