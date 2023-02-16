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

  :hook
  (vterm-mode . (lambda ()
                  (setq-local show-trailing-whitespace nil))))

(global-set-key [(super t)] #'(lambda ()
                                (interactive)
                                (my/named-vterm-for-current-project "primary")))

(global-set-key [(super T)] #'(lambda ()
                                (interactive)
                                (my/named-vterm-for-current-project "secondary")))

(global-set-key (kbd "M-t" ) #'(lambda ()
                                (interactive)
                                (my/named-vterm-for-current-project "tertiary")))

(global-set-key (kbd "M-T") 'my/new-vterm-for-current-project)

(defun my/vterm-for-current-project ()
  "Switch to the project-specific vterm terminal buffer if it
already exists, or create a new vterm terminal buffer named after
the current project."
  (interactive)
  (projectile-run-vterm))

(defun my/named-vterm-for-current-project (name)
  "Switch to the project-specific vterm terminal buffer identified
by NAME if it already exists, or create a new vterm terminal
buffer named after the current project and the provided NAME."
  (let* ((project (projectile-acquire-root))
         (name (format "vterm[%s]" name))
         (buffer (projectile-generate-process-name name nil project)))
    (unless (buffer-live-p (get-buffer buffer))
      (projectile-with-default-dir project
        (vterm buffer)))
    ;; If the buffer is already visible, switch to it. Otherwise, open the
    ;; buffer in the current window.
    (cond ((get-buffer-window buffer 0) (pop-to-buffer buffer nil))
          (t (pop-to-buffer-same-window buffer nil)))))

(defun my/new-vterm-for-current-project ()
  "Open a new vterm terminal named after the current project.
Unlike `my/vterm-for-current-project`, this command always opens
a brand-new terminal (i.e., it does not switch to the project
specific term buffer if it already exists)."
  (interactive)
  (vterm (concat "*vterm " (projectile-project-name) "*")))

(defun my/new-named-vterm (term-name)
  "Open a vterm terminal with buffer name TERM-NAME."
  (interactive "sTerminal name: ")
  (vterm (concat "*vterm " term-name "*")))

(defun my/new-named-vterm-other-window (term-name)
  "Open a vterm terminal with buffer name TERM-NAME in the other
window."
  (interactive "sTerminal name: ")
  (vterm-other-window (concat "*vterm " term-name "*")))
