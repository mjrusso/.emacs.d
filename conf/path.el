;; Set $MANPATH, $PATH and exec-path from the shell, but only on OS X and Linux.
;; See https://github.com/purcell/exec-path-from-shell for more details.
;;
;; Also see https://github.com/purcell/envrc/issues/92 for considerations
;; related to Nix, nix-direnv, and Fish.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (dolist (var '("SSH_AUTH_SOCK"
                   "SSH_AGENT_PID"
                   "XDG_DATA_DIRS"
                   "XDG_CONFIG_DIRS"
                   "NIX_USER_PROFILE_DIR"
                   "NIX_SSL_CERT_FILE"
                   "NIX_PROFILES"
                   "NIX_PATH"
                   "__fish_nixos_env_preinit_sourced"
                   "__NIX_DARWIN_SET_ENVIRONMENT_DONE"
                   "__HM_SESS_VARS_SOURCED"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

;; https://github.com/wbolster/emacs-direnv
(use-package direnv
  :disabled

  ;; This fork adds TRAMP support:
  ;;
  ;; - https://github.com/wbolster/emacs-direnv/pull/68
  ;; - https://github.com/wbolster/emacs-direnv/issues/47
  ;;
  ;; However, I'm not using it right now because it seems to be causing issues
  ;; with the PATH not updating properly on unload:
  ;;
  ;; - https://github.com/wbolster/emacs-direnv/pull/68#issuecomment-1441943593
  ;;
  ;; Also see: https://github.com/purcell/envrc/issues/27

  ;; :straight (direnv :type git :host github :repo "wbolster/emacs-direnv" :branch "main"
  ;;                   :fork (:host github :repo "siddharthverma314/emacs-direnv" :branch "master"))

  :config
  (add-to-list 'direnv-non-file-modes 'vterm-mode)

  (direnv-mode)
  )

;; https://github.com/purcell/envrc
(use-package envrc
  :init (setq envrc-debug t)
  :hook (after-init . envrc-global-mode)
  )

