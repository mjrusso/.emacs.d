;; Avy: jump to visible text using a character-based decision tree.
;;
;; https://github.com/abo-abo/avy

(use-package avy
  :bind (("C-'" . avy-goto-char-timer)
         ("C-;" . avy-goto-line)))

;; ace-window: improved window selection.
;;
;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :bind ("C-x o" . ace-window))
