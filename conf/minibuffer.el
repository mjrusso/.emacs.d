;; https://github.com/minad/vertico
(use-package vertico

  ;; Straight needs some help here, because additional extensions are included
  ;; in the same package/repository as Vertico itself. See:
  ;;
  ;; - https://github.com/raxod502/straight.el/issues/819#issuecomment-882039946
  ;; - https://kristofferbalintona.me/posts/vertico-marginalia-all-the-icons-completion-and-orderless/
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-buffer
                                vertico-directory
                                vertico-flat
                                vertico-grid
                                vertico-indexed
                                vertico-mouse
                                vertico-multiform
                                vertico-quick
                                vertico-repeat
                                vertico-reverse
                                vertico-unobtrusive
                                ))
  :init

  (vertico-mode)

  :config

  (setq vertico-count 20)

  :bind (:map vertico-map
              ("C-s" . vertico-next)
              ("C-r" . vertico-previous)))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; https://github.com/tumashu/vertico-posframe/
(use-package vertico-posframe
  :disabled
  :after (vertico posframe)
  :config
  (setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
  (setq vertico-posframe-min-width 212)
  (vertico-posframe-mode 1))

;; https://github.com/minad/marginalia
(use-package marginalia
  :config
  (setq marginalia-align 'left)
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; https://github.com/oantolin/embark/
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Replace the key help with a completing-read interface.
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
