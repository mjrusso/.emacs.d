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

(defun my/resize-frame-to-middle-of-screen (frame)
  (let* ((width (floor (/ (x-display-pixel-width) 2.0)))
         (height (floor (/ (x-display-pixel-height) 1.25)))
         (x (+ 0 (floor (* width 0.5))))
         (y (+ 0 (floor (* height 0.1)))))
    (set-frame-position (selected-frame) x y)
    (set-frame-size (selected-frame) width height t)))

;; On startup, tweak the size and placement of the initial frame (unless Emacs
;; is running in a terminal).
(when (display-graphic-p)
  (let ((width (floor (/ (x-display-pixel-width) 2))))
    (if (< width 1000)
        (my/maximize-frame (selected-frame))
      (my/resize-frame-to-middle-of-screen (selected-frame)))))
