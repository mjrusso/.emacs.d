;; - https://github.com/atomontage/xterm-color
(use-package xterm-color)

;; Emacs-libvterm: a terminal emulator, based on libvterm.
;;
;; Details: https://github.com/akermu/emacs-libvterm
;;
;; Important: requires shell-side configuration! See
;; https://github.com/akermu/emacs-libvterm#shell-side-configuration for
;; details.
(use-package vterm
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)
  :hook
  (vterm-mode . (lambda ()
                  (setq-local show-trailing-whitespace nil))))

(global-set-key [(super t)] 'my/vterm-for-current-project)

(global-set-key [(super T)] 'my/new-vterm-for-current-project)

(defun my/vterm-for-current-project ()
  "Switch to the project-specific vterm terminal buffer if it
already exists, or create a new vterm terminal buffer named after
the current project."
  (interactive)
  (projectile-run-vterm))

(defun my/new-vterm-for-current-project ()
  "Open a new vterm terminal named after the current project.
Unlike `my/vterm-for-current-project`, this command always opens
a brand-new terminal (i.e., it does not switch to the project
specific term buffer if it already exists)."
  (interactive)
  (vterm (concat "*vterm " (projectile-project-name) "*")))

(defun my/new-named-vterm (term-name)
  "Open a vterm terminal with buffer name TERM-NAME."
  (interactive "sTerminal name: ")
  (vterm (concat "*vterm " term-name "*")))

(defun my/new-named-vterm-other-window (term-name)
  "Open a vterm terminal with buffer name TERM-NAME in the other
window."
  (interactive "sTerminal name: ")
  (vterm-other-window (concat "*vterm " term-name "*")))

(major-mode-hydra-define+ (vterm-mode)
  (:color teal :quit-key "q")
  ("Quick Action"
   (("cc" vterm-copy-mode "toggle copy mode" :toggle t :exit t)
    ("cs" vterm-clear-scrollback "clear scrollback"))))
