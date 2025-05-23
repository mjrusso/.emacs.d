(use-package emacs

  :init

  ;; Create the necessary directories (for backups, auto-saves, etc.), unless
  ;; they already exist.
  (dolist (dir (list "backups" "auto-saves" "locks"))
    (let ((full-path (concat user-emacs-directory dir)))
      (unless (file-directory-p full-path)
        (make-directory full-path t))))

  (setq visible-bell t
        inhibit-startup-message t
        color-theme-is-global t
        apropos-do-all t
        sentence-end-double-space nil
        require-final-newline t
        indent-tabs-mode nil
        shift-select-mode nil
        mouse-yank-at-point t
        ediff-window-setup-function 'ediff-setup-windows-plain
        save-place-file (concat user-emacs-directory "places")
        save-interprogram-paste-before-kill t
        load-prefer-newer t
        backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
        auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-saves/") t))
        lock-file-name-transforms `((".*" ,(concat user-emacs-directory "locks/") t))
        diff-switches "-u"
        epa-armor t
        tls-checktrust 'ask
        el-get-allow-insecure nil
        gnutls-verify-error t
        network-security-level 'high)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  (setq-default indent-tabs-mode nil
                fill-column 79
                indicate-empty-lines t
                indicate-buffer-boundaries nil)

  (add-to-list 'safe-local-variable-values '(lexical-binding . t))
  (add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

  (global-unset-key (kbd "s-s"))
  (global-unset-key (kbd "s-g"))
  (global-unset-key (kbd "s-f"))
  (global-unset-key (kbd "s-t"))
  (global-unset-key (kbd "s-o"))
  (global-unset-key (kbd "s-l"))

  ;; These tweaks are based on the recommendations by lsp-doctor.
  (setq read-process-output-max (* 1024 1024)) ;; 1 MB
  (setq gc-cons-threshold (* 100 (* 1024 1024))) ;; 100 MB

  )
