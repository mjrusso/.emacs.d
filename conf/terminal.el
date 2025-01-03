;; WIP support for improving Emacs in the terminal.
;;
;;(With thanks to @drusso for sharing his config.)

;; (use-package emacs
;;   :config
;;   (setq
;;    select-enable-clipboard nil
;;    select-enable-primary nil))

;; (use-package emacs
;;   :bind (("M-W" . clipboard-kill-ring-save)))

(use-package xclip :ensure t)
(use-package clipetty :ensure t)

(if (not (display-graphic-p))
    (if (or (executable-find "pbcopy")
            (executable-find "xclip"))
        ;; xclip uses external programs such as xclip and pbcopy.
        ;;
        (use-package xclip
          :init
          (xclip-mode 1))

      ;; https://github.com/spudlyo/clipetty
      ;;
      ;; `clipetty' uses OSC 52 escape sequences to send text killed in Emacs
      ;; to the system clipboard.
      (use-package clipetty
        :ensure t
        :hook (after-init . global-clipetty-mode)
        )))


;; Add support for the Kitty Keyboard protocol.
;;
;; Protocol: https://sw.kovidgoyal.net/kitty/keyboard-protocol/
;; kkp.el: https://github.com/benjaminor/kkp
(use-package kkp
  :ensure t
  :config
  (global-kkp-mode +1)
  )

(defun my/tty-setup-hook ()
  (message "Running tty-setup hook..."))

(use-package emacs
  :config
  (setq xterm-set-window-title t)
  (when (not (window-system))
    (if (not (eq (tty-display-color-cells) 16777216))
        ;; https://chadaustin.me/2024/01/truecolor-terminal-emacs/
        (warn "TTY does not appear to support 24 bit colour")
      ))
  :hook
  (tty-setup . my/tty-setup-hook)
  )

;; (set 'xterm-standard-colors
;;   '(("black"          0 (  0   0   0))
;;     ("red"            1 (255   0   0))
;;     ("green"          2 (  0 255   0))
;;     ("yellow"         3 (255 255   0))
;;     ("blue"           4 (  0   0 255))
;;     ("magenta"        5 (255   0 255))
;;     ("cyan"           6 (  0 255 255))
;;     ("white"          7 (255 255 255))
;;     ("brightblack"    8 (127 127 127))
;;     ("brightred"      9 (255   0   0))
;;     ("brightgreen"   10 (  0 255   0))
;;     ("brightyellow"  11 (255 255   0))
;;     ("brightblue"    12 (92   92 255))
;;     ("brightmagenta" 13 (255   0 255))
;;     ("brightcyan"    14 (  0 255 255))
;;     ("brightwhite"   15 (255 255 255)))
;;   )
