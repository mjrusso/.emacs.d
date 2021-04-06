(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      apropos-do-all t
      sentence-end-double-space nil
      require-final-newline t
      indent-tabs-mode nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      save-interprogram-paste-before-kill t
      load-prefer-newer t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u"
      epa-armor t
      tls-checktrust 'ask
      el-get-allow-insecure nil
      gnutls-verify-error t
      network-security-level 'high)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
