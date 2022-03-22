;; Avy: jump to visible text using a character-based decision tree.
;;
;; https://github.com/abo-abo/avy

(use-package avy
  :bind (("s-j" . avy-goto-char-timer)
         ("C-'" . avy-goto-line)))

;; Switch to the last active window. (Repeatedly calling this function cycles
;; between the two most recently used windows.)
;;
;; Adapted from: https://emacs.stackexchange.com/a/7411
(defun my/switch-to-last-active-window ()
  (interactive)
  (let ((window (get-mru-window t t t)))
    (unless window (error "Cannot switch windows, because last window not found"))
    (let ((frame (window-frame window)))
      (select-frame-set-input-focus frame)
      (select-window window))))

(global-set-key (kbd "C-x i") #'my/switch-to-last-active-window)

;; ace-window: improved window selection.
;;
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :config
  (ace-window-display-mode)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-background nil)
  :bind ("C-x o" . ace-window))
