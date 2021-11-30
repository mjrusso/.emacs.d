;; Enable mouse support in the terminal.
(use-package mouse
  :straight nil
  :init (setq mouse-sel-mode t)
  :config (xterm-mouse-mode t))

(global-set-key [mouse-4] '(lambda ()
                            (interactive)
                            (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
                            (interactive)
                            (scroll-up 1)))
(defun track-mouse (e))
