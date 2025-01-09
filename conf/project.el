;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el
(use-package project

  :config
  (define-key global-map (kbd "C-c p") project-prefix-map)

  :init
  (defun my/project-switch-project (&optional project-root)
    "Switch to a project, optionally specifying PROJECT-ROOT directly."
    (interactive)
    (let* ((project (or project-root (project-prompt-project-dir)))
           (default-directory project)
           (proj (project-current))
           (project-name (file-name-nondirectory (directory-file-name project))))
      (when project
        ;; Set the frame name as the project name (because the frame name is
        ;; displayed in the modeline), but do *not* create a new frame.
        ;;
        ;; This setup is optimized for TTY Emacs, where we simply start new
        ;; emacsclient instances for each project/context. (Each emacsclient
        ;; instance automatically gets a new frame, so `beframe' will
        ;; automatically provide frame-level isolation.)
        ;;
        ;; See additional discussion in the comments on this commit:
        ;; https://github.com/mjrusso/.emacs.d/commit/2399bdbfb166a96e46d0bd8854eac2d7e562957e
        (set-frame-name project-name)
        (delete-other-windows)
        (if (eq (car proj) 'vc)
            (magit-status)
          (project-dired)))))

  (defun my/is-project-directory-p (directory)
    "Check if DIRECTORY is a project directory using project.el.
Returns t if directory is recognized as a project, nil otherwise."
    (when (and directory (file-directory-p directory))
      (let ((default-directory (expand-file-name directory)))
	(when-let ((project (project-current nil)))
          t))))

  (defun my/maybe-open-project (directory)
    "Switch to a project at DIRECTORY if it is a project, otherwise open the scratch buffer."
    (interactive)
    (if (my/is-project-directory-p directory)
        (my/project-switch-project directory)
      (switch-to-buffer "*scratch*")))

  (defun my/open-project ()
    (interactive)
    (my/project-switch-project))

  (defun my/open-project-by-name (project-root)
    (my/project-switch-project project-root))

  :bind
  ("C-c f" . project-find-file)
  ("C-c d" . project-find-dir)
  ("C-c +" . my/open-project)

  )
