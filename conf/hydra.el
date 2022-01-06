(use-package major-mode-hydra
  :demand t
  :bind
  ("M-SPC" . major-mode-hydra))

(use-package hydra-posframe
  :straight (:host github :repo "Ladicle/hydra-posframe"
             :fork (:host github :repo "jerrypnz/hydra-posframe"))
  :after (posframe hydra)
  :hook (after-init . hydra-posframe-enable)
  :config
  (setq hydra-posframe-poshandler 'posframe-poshandler-frame-center)
  (setq hydra-posframe-parameters '((alpha 100 100)
                                    (left-fringe . 10)
                                    (right-fringe . 10)))
  (set-face-attribute 'hydra-posframe-face nil :foreground "white" :background "#120022"))
