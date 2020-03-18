;; Set $MANPATH, $PATH and exec-path from the shell, but only on OS X and Linux.
;; See https://github.com/purcell/exec-path-from-shell for more details.

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
