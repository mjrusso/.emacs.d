;; Avy: jump to visible text using a character-based decision tree.
;;
;; https://github.com/abo-abo/avy

(use-package avy
  :ensure t
  :defer nil
  :bind (("C-;" . avy-goto-char-timer)
         ("C-'" . avy-goto-line)))
