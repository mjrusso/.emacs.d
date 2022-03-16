;; https://github.com/minad/vertico
(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-s" . vertico-next)
              ("C-r" . vertico-previous)))

(use-package vertico-directory
  :after vertico
  ;; Straight needs some help here, because vertico-directory is included in
  ;; the same repo as vertico. See:
  ;; https://github.com/raxod502/straight.el/issues/819
  :straight nil
  :load-path "straight/build/vertico/extensions"
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; https://github.com/minad/marginalia
(use-package marginalia
  :config
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
