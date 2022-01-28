;; Configure Fountain Mode.
(use-package fountain-mode
  :mode "\\.fountain\\'"
  :init
  ;; Don't indent elements.
  (setq fountain-indent-elements nil)
  ;; Use visual line mode by default for Fountain documents.
  (add-hook 'fountain-mode-hook 'turn-off-auto-fill)
  (add-hook 'fountain-mode-hook 'turn-on-visual-line-mode))

(defun hide-fringes ()
  (interactive)
  ;; (set-face-attribute 'fringe nil :background "#272822" :foreground "#FFFFFF")
  (set-window-fringes nil 0 0))

(use-package olivetti
  :init
  (add-hook 'fountain-mode-hook 'olivetti-mode)
  (add-hook 'fountain-mode-hook 'hide-fringes))

;; Automatically delete trailing whitespace, except in the case of Fountain
;; Mode. (Fountain relies on trailing whitespace in certain circumstances.)
(add-hook 'before-save-hook 'my/nuke-trailing-whitespace)
(defun my/nuke-trailing-whitespace ()
  ;; Note additional option: https://github.com/lewang/ws-butler
  (when (not (derived-mode-p 'fountain-mode))
    (delete-trailing-whitespace)))
