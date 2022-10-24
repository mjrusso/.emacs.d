;; Set $MANPATH, $PATH and exec-path from the shell, but only on OS X and Linux.
;; See https://github.com/purcell/exec-path-from-shell for more details.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; https://github.com/wbolster/emacs-direnv
(use-package direnv
 :config
 (direnv-mode))
