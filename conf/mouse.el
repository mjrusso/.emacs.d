;; Enable smooth scrolling.
(pixel-scroll-precision-mode 1)

;; Enable mouse support in the terminal.
(use-package mouse
  :straight nil
  :init (setq mouse-sel-mode t)
  :config (xterm-mouse-mode t))

;; Disable the trackpad pinch gesture (normally, this gesture calls
;; `text-scale-pinch').
(global-set-key [pinch] #'(lambda () (interactive)))

(global-set-key [mouse-4] #'(lambda ()
                              (interactive)
                              (scroll-down 1)))

(global-set-key [mouse-5] #'(lambda ()
                              (interactive)
                              (scroll-up 1)))
(defun track-mouse (e))
