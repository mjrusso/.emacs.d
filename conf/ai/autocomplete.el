;; Wingman: LLM-assisted text completion
;;
;; Requires running llama.cpp server running a model that supports FIM
;; (fill-in-the-middle) completion.
;;
;; https://github.com/mjrusso/wingman
(use-package wingman
  :straight (:type git :local-repo "~/git/github.com/mjrusso/wingman")
  :ensure t
  :defer t

  :init

  (setq wingman-prefix-key (kbd "C-c a"))

  :hook (prog-mode . wingman-mode)

  :bind

  (:map wingman-mode-prefix-map
        ("z" . wingman-fim-inline)
        ("x" . wingman-debug-completion)
        :map wingman-mode-completion-transient-map
        ("TAB" . wingman-accept-full))

  :config

  (setq wingman-log-level 4)
  (setq wingman-key-accept-full (kbd "TAB"))
  (setq wingman-llama-endpoint "http://127.0.0.1:8012/infill")
  (setq wingman-ring-n-chunks 32)

  ;; assumes use of Modus Themes; substitute with preferred color scheme
  (set-face-attribute 'wingman-overlay-face nil
                      :foreground  (modus-themes-get-color-value 'red-warmer)
                      :background  (modus-themes-get-color-value 'bg-inactive))

  ;; don't provide completions in files that typically contain secrets
  (add-to-list 'wingman-disable-predicates
               (lambda ()
                 (or (derived-mode-p 'envrc-file-mode)
                     (derived-mode-p 'direnv-envrc-mode)
                     (when buffer-file-name
                       (let ((fname (file-name-nondirectory buffer-file-name)))
                         (or (string-equal ".env" fname)
                             (string-equal ".envrc" fname)
                             (string-prefix-p ".localrc" fname))))))))
