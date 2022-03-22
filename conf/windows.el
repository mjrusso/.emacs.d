;; When splitting a window, prefer a horizontal stack rather than a vertical
;; stack (i.e., create the buffer to the right rather than below).
(setq split-width-threshold 80)
(setq split-height-threshold nil)

;; Avoid resizing.
; (customize-set-variable 'even-window-sizes nil)

;; From https://github.com/nex3/perspective-el#some-musings-on-emacs-window-layouts:
;;
;; > 1) Tell `display-buffer' to reuse existing windows as much as possible,
;; > including in other frames. For example, if there is already a *compilation*
;; > buffer in a visible window, switch to that window. This means that Emacs
;; > will usually switch windows in a "do what I mean" manner for a warmed-up
;; > workflow (one with, say, a couple of source windows, a compilation output
;; > window, and a Magit window).
;; >
;; > 2) Prevent splits by telling `display-buffer' to switch to the target
;; > buffer in the current window. For example, if there is no *compilation*
;; > buffer visible, then the buffer in whichever window was current when
;; > compile was run will be replaced with *compilation*. This may seem
;; > intrusive, since it changes out the current buffer, but keep in mind that
;; > most buffers popped up in this manner are easy to dismiss, either with a
;; > dedicated keybinding (often 'q') or the universally-applicable
;; > `kill-buffer'. This is easier than restoring window arrangements. It is
;; > also easier to handle for pre-arranged window layouts, since the
;; > appropriate command can simply be run in a window prepared for it in
;; > advance. (If this is a step too far, then replace
;; > `display-buffer-same-window' with `display-buffer-pop-up-window'.)

(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-pop-up-window)
    (reusable-frames . t)))
