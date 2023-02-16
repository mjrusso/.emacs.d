(use-package tramp
  :defer t
  :straight nil

  :config
  (require 'tramp)

  (setq tramp-default-method "ssh")

  ;; Set Tramp's verbosity (0 = silent, 1 = error, 2 = warning, etc.).
  (setq tramp-verbose 1)

  ;; Use the remote file cache (which potentially improves the speed of some
  ;; Tramp operations).
  (setq remote-file-name-inhibit-cache nil)

  ;; Use the value of PATH assigned to the remote user by the remote host. (See
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Remote-programs.html
  ;; for more details on how this works.)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Configure connection-local profile variables.
  ;;
  ;; To use these when connecting to a given remote host, set
  ;; `connection-local-set-profiles' appropriately.
  ;;
  ;; For example, to use the fish shell on a machine with hostname "example":
  ;;
  ;;     (connection-local-set-profiles
  ;;      '(:application tramp :protocol "ssh" :machine "example")
  ;;      'remote-fish)
  ;;
  ;; In my case, `connection-local-set-profiles' are configured in private
  ;; configuration files (not committed to this repository).

  (connection-local-set-profile-variables
   'remote-fish
   '((explicit-shell-file-name . "/usr/local/bin/fish")))

  (connection-local-set-profile-variables
   'remote-fish-alt
   '((explicit-shell-file-name . "/opt/homebrew/bin/fish")))

  (connection-local-set-profile-variables
   'remote-zsh
   '((explicit-shell-file-name . "/bin/zsh")))

  (connection-local-set-profile-variables
   'remote-bash
   '((explicit-shell-file-name . "/bin/bash")))

  )
