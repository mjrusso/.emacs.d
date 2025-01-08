;; Emacs-libvterm: a terminal emulator, based on libvterm.
;;
;; Details: https://github.com/akermu/emacs-libvterm
;;
;; Important: requires shell-side configuration! See
;; https://github.com/akermu/emacs-libvterm#shell-side-configuration for
;; details.
(use-package vterm
  :commands vterm
  :custom
  (vterm-max-scrollback 10000)

  (vterm-clear-scrollback-when-clearing nil)

  ;; TODO this might not be necessary with a newer release of vterm; see:
  ;; https://github.com/akermu/emacs-libvterm/pull/652
  (vterm-tramp-shells '(
                        ("ssh" "/opt/homebrew/bin/fish")
                        ("ssh" "/usr/local/bin/fish")
                        ("ssh" "/bin/zsh")
                        ("ssh" "/bin/bash")

                        ("sudo" "/opt/homebrew/bin/fish")
                        ("sudo" "/usr/local/bin/fish")
                        ("sudo" "/bin/zsh")
                        ("sudo" "/bin/bash")

                        ("docker" "/bin/sh")
                        ))

  :hook
  (vterm-mode . (lambda ()
                  (setq-local show-trailing-whitespace nil))))


(global-set-key (kbd "C-c t") #'(lambda ()
                                  (interactive)
                                  (my/named-vterm-for-current-project "primary")))

(global-set-key (kbd "C-c T") #'(lambda ()
                                  (interactive)
                                  (my/named-vterm-for-current-project "secondary")))

(global-set-key (kbd "M-t" ) #'(lambda ()
                                 (interactive)
                                 (my/named-vterm-for-current-project "tertiary")))

(global-set-key (kbd "M-T") 'my/new-vterm-for-current-project)

(defun my/named-vterm-for-current-project (name)
  "Switch to the project-specific vterm terminal buffer identified
by NAME if it already exists, or create a new vterm terminal
buffer named after the current project and the provided NAME."
  (let* ((project (project-current))
         (project-root (project-root project))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (buffer-name (format "*vterm-%s[%s]*" name project-name)))
    (unless (buffer-live-p (get-buffer buffer-name))
      (let ((default-directory project-root))
        (vterm buffer-name)))
    ;; If the buffer is already visible, switch to it. Otherwise, open the
    ;; buffer in the current window.
    (cond ((get-buffer-window buffer-name 0) (pop-to-buffer buffer-name nil))
          (t (pop-to-buffer-same-window buffer-name nil)))))

(defun my/new-vterm-for-current-project ()
  "Open a new vterm terminal named after the current project.
Unlike `my/vterm-for-current-project`, this command always opens
a brand-new terminal (i.e., it does not switch to the project
specific term buffer if it already exists)."
  (interactive)
  (vterm (concat "*vterm " (projectile-project-name) "*")))

(defun my/new-vterm-for-current-project ()
  "Open a new vterm terminal named after the current project.
Unlike `my/vterm-for-current-project', this command always opens
a brand-new terminal (i.e., it does not switch to the project
specific term buffer if it already exists)."
  (interactive)
  (let* ((project (project-current))
         (project-name (file-name-nondirectory
                        (directory-file-name
                         (project-root project)))))
    (vterm (format "*vterm %s*" project-name))))


(defun my/new-named-vterm (term-name)
  "Open a vterm terminal with buffer name TERM-NAME."
  (interactive "sTerminal name: ")
  (vterm (format "*vterm %s*" term-name)))

(defun my/new-named-vterm-other-window (term-name)
  "Open a vterm terminal with buffer name TERM-NAME in the other
window."
  (interactive "sTerminal name: ")
  (vterm-other-window (format "*vterm %s*" term-name)))
