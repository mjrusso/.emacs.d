;; Set $MANPATH, $PATH and exec-path from the shell, but only on OS X and Linux.
;; See https://github.com/purcell/exec-path-from-shell for more details.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; https://github.com/wbolster/emacs-direnv
(use-package direnv

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
