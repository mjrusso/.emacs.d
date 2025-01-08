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
           (proj (project-current)))
      (when project
        (if (eq (car proj) 'vc)
            (magit-status)
          (project-dired)))))

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
