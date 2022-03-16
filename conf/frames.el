(defun my/resize-frame-to-right-half-of-screen (frame)
  (let* ((width (floor (/ (x-display-pixel-width) 2)))
         (height (floor (/ (x-display-pixel-height) 1)))
         (x (- (x-display-pixel-width) width (floor (* width 0.02))))
         (y (- (x-display-pixel-height) height)))
    (set-frame-position frame x y)
    (set-frame-size frame width height t)))

(defun my/resize-current-frame-to-right-half-of-screen ()
  (interactive)
  (my/resize-frame-to-right-half-of-screen (selected-frame)))

(defun my/maximize-frame (frame)
  (set-frame-parameter frame 'fullscreen 'maximized))

(defun my/maximize-current-frame ()
  (interactive)
  (my/maximize-frame (selected-frame)))

(defalias 'my/resize-current-frame-to-full-screen 'my/maximize-current-frame)

;; Maximize the initial frame on startup.
(when window-system (my/maximize-frame nil))

;; Make new frames (not the one on startup) a specific fixed size.
(add-hook 'after-make-frame-functions 'my/resize-frame-to-right-half-of-screen)
