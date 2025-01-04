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

;; Zellij supports the "disambiguate escape codes" component of the Kitty
;; Keyboard protocol, but **not** "report alternate keys". (See
;; <https://github.com/zellij-org/zellij/issues/3789> for more details and
;; discussion.)
;;
;; An implication is explained in the kpp.el README:
;;
;; > Note that when you activate only `disambiguate-escape-codes`, the terminal
;; > reports shifted keypresses which involve another modifier by sending the
;; > modifiers with the base layout of the key.
;; >
;; > This means "M-S-." (Meta-Shift-.) is not translated to "M-:" (on a German
;; > keyboard) and Emacs will probably not find the proper keybinding.
;;
;; As a workaround to Zellij's lack of support for `disambiguate-escape-codes`,
;; we simply add relevant entries to the key translation map. For example, for
;; an English layout, we map "M-S-." to "M-->".
(use-package emacs
  :config
  (define-key key-translation-map (kbd "M-S-`") (kbd "M-~"))
  (define-key key-translation-map (kbd "M-S-1") (kbd "M-!"))
  (define-key key-translation-map (kbd "M-S-2") (kbd "M-@"))
  (define-key key-translation-map (kbd "M-S-3") (kbd "M-#"))
  (define-key key-translation-map (kbd "M-S-4") (kbd "M-$"))
  (define-key key-translation-map (kbd "M-S-5") (kbd "M-%"))
  (define-key key-translation-map (kbd "M-S-6") (kbd "M-^"))
  (define-key key-translation-map (kbd "M-S-7") (kbd "M-&"))
  (define-key key-translation-map (kbd "M-S-8") (kbd "M-*"))
  (define-key key-translation-map (kbd "M-S-9") (kbd "M-("))
  (define-key key-translation-map (kbd "M-S-0") (kbd "M-)"))
  (define-key key-translation-map (kbd "M-S--") (kbd "M-_"))
  (define-key key-translation-map (kbd "M-S-=") (kbd "M-+"))
  (define-key key-translation-map (kbd "M-S-[") (kbd "M-{"))
  (define-key key-translation-map (kbd "M-S-]") (kbd "M-}"))
  (define-key key-translation-map (kbd "M-S-\\") (kbd "M-|"))
  (define-key key-translation-map (kbd "M-S-;") (kbd "M-:"))
  (define-key key-translation-map (kbd "M-S-'") (kbd "M-\""))
  (define-key key-translation-map (kbd "M-S-,") (kbd "M-<"))
  (define-key key-translation-map (kbd "M-S-.") (kbd "M->"))
  (define-key key-translation-map (kbd "M-S-/") (kbd "M-?")))

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
