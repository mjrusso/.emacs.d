(use-package emacs
  :init

  ;; Note that the "EMACS" ASCII art *actually* looks like this:
  ;;
  ;;     _______   _____ ______   ________  ________  ________
  ;;    |\  ___ \ |\   _ \  _   \|\   __  \|\   ____\|\   ____\
  ;;    \ \   __/|\ \  \\\__\ \  \ \  \|\  \ \  \___|\ \  \___|_
  ;;     \ \  \_|/_\ \  \\|__| \  \ \   __  \ \  \    \ \_____  \
  ;;      \ \  \_|\ \ \  \    \ \  \ \  \ \  \ \  \____\|____|\  \
  ;;       \ \_______\ \__\    \ \__\ \__\ \__\ \_______\____\_\  \
  ;;        \|_______|\|__|     \|__|\|__|\|__|\|_______|\_________\
  ;;                                                   \|_________|
  ;;
  ;; ...but the '\' characters are escaped in the string below, messing up the
  ;;    formatting.

  (setq initial-scratch-message (purecopy "\
;;     _______   _____ ______   ________  ________  ________
;;    |\\  ___ \\ |\\   _ \\  _   \\|\\   __  \\|\\   ____\\|\\   ____\\
;;    \\ \\   __/|\\ \\  \\\\\\__\\ \\  \\ \\  \\|\\  \\ \\  \\___|\\ \\  \\___|_
;;     \\ \\  \\_|/_\\ \\  \\\\|__| \\  \\ \\   __  \\ \\  \\    \\ \\_____  \\
;;      \\ \\  \\_|\\ \\ \\  \\    \\ \\  \\ \\  \\ \\  \\ \\  \\____\\|____|\\  \\
;;       \\ \\_______\\ \\__\\    \\ \\__\\ \\__\\ \\__\\ \\_______\\____\\_\\  \\
;;        \\|_______|\\|__|     \\|__|\\|__|\\|__|\\|_______|\\_________\\
;;                                                   \\|_________|


(message  \"🚀\")


")))

;; Adapted from https://github.com/emacs-dashboard/emacs-dashboard
(defun my/get-emacs-load-time-and-package-count ()
  (let ((package-count 0) (time (emacs-init-time "%.2f")))
    (when (bound-and-true-p package-alist)
      (setq package-count (length package-activated-list)))
    (when (boundp 'straight--profile-cache)
      (setq package-count (+ (hash-table-count straight--profile-cache) package-count)))
    (if (zerop package-count)
        (format "Emacs loaded in %s seconds" time)
      (format "%d packages loaded in %s seconds" package-count time))))

(defun my/display-emacs-load-time-and-package-count ()
  "Log how long Emacs took to load (and the number of packages that were loaded) to the minibuffer."
  (interactive)
  (message (my/get-emacs-load-time-and-package-count)))

(add-hook 'emacs-startup-hook #'my/display-emacs-load-time-and-package-count)

(defun my/construct-default-workspace-and-window-setup ()
  "Called by emacs-startup-hook to set up my initial window configuration."
  ;; (my/workspace:dashboard)
  )

(add-hook 'emacs-startup-hook #'my/construct-default-workspace-and-window-setup)
