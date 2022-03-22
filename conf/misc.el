;; Never trigger the alarm bell.
(setq ring-bell-function
      (lambda ()))

;; Do not scroll dramatically when moving only one line at a time.
(setq-default scroll-conservatively 1)

;; Enable cua mode, but only for rectangles.
;; (C-RET will start a rectangular selection.)
(setq cua-enable-cua-keys nil)
(cua-mode t)

(use-package winner
  :straight nil
  :init
  (setq winner-dont-bind-my-keys t)
  :bind (("C-c u" . winner-undo)
         ("C-c r" . winner-redo))
  :config
  (winner-mode))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(use-package uniquify
  :straight nil
  :init (setq uniquify-buffer-name-style 'post-forward-angle-brackets
              uniquify-min-dir-content 2))

(use-package htmlize)

;; Don't open a new buffer for every visited directory.
;;
;; Excerpted from: https://blog.sumtypeofway.com/posts/emacs-config.html
(use-package dired
  :straight nil
  :init
  (defun my/dired-up-directory-same-buffer ()
    "Go up in the same buffer."
    (find-alternate-file ".."))
  (defun my/dired-mode-hook ()
    (put 'dired-find-alternate-file 'disabled nil) ; Disables the warning.
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "^") 'my/dired-up-directory-same-buffer))
  (add-hook 'dired-mode-hook #'my/dired-mode-hook)
  (setq dired-use-ls-dired nil)
  )

;; Persist a list of recently-opened files.
;; - https://www.emacswiki.org/emacs/RecentFiles
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
        recentf-max-saved-items 100
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude recentf-save-file)
  (recentf-mode +1)
  (run-at-time nil ;; Periodically save the list of files.
               (* 5 60)
               (lambda ()
                 (let ((inhibit-message t))
                   (recentf-save-list)))))

;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file.
;; - https://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :init
  (save-place-mode 1))

;; Persist M-x command history (and other minibuffer histories) across
;; shutdowns.
;; - https://www.emacswiki.org/emacs/SaveHist
(use-package savehist
  :init
  (savehist-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package imenu
  :init
  (setq-default imenu-auto-rescan t))

(use-package imenu-list)

;; The command to use to open a file using its default external program.
(setq my/open-command
      (pcase system-type
        (`darwin "open")
        ((or `gnu `gnu/linux `gnu/kfreebsd) "xdg-open")))

(defun my/open-file-with (arg)
  "Open visited file in default external program.
When in dired mode, open file under the cursor.
With a prefix ARG always prompt for command to use."
  (interactive "P")
  (let* ((current-file-name
          (if (eq major-mode 'dired-mode)
              (dired-get-file-for-visit)
            buffer-file-name))
         (open my/open-command)
         (program (if (or arg (not open))
                      (read-shell-command "Open current file with: ")
                    open)))
    (call-process program nil 0 nil current-file-name)))

(defun my/open-directory ()
  "Open directory of visited file in default external program."
  (interactive)
  (let* ((current-directory (file-name-directory buffer-file-name)))
    (call-process my/open-command nil 0 nil current-directory)))

(defun my/open-parent-directory ()
  "Open parent directory of visited file in default external program."
  (interactive)
  (let* ((parent-directory
          (file-name-directory
           (directory-file-name
            (file-name-directory
             (directory-file-name buffer-file-name))))))
    (call-process my/open-command nil 0 nil parent-directory)))

(global-set-key (kbd "C-c o f") 'my/open-file-with)
(global-set-key (kbd "C-c o d") 'my/open-directory)
(global-set-key (kbd "C-c o p") 'my/open-parent-directory)

(defun my/print-env ()
  (interactive)
  (message "Shell: %s\nPath: %s"
           (getenv "SHELL")
           (getenv "PATH")))
