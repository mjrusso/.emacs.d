(use-package projectile
  :init
  (setq projectile-use-git-grep t)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-require-project-root nil)
  (setq projectile-completion-system 'default)
  (projectile-mode +1)

  :bind (
         ("s-f" . projectile-find-file)
         ("s-d" . projectile-find-dir)

         ;; S-R: search entire project
         ;; C-u S-R (or M-1 S-R): allows user to specify search directory
         ;;
         ;; To display search results in the minibuffer, invoke
         ;; `M-x consult-ripgrep` directly.
         ("s-R" . projectile-ripgrep)
         )

  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-globally-ignored-directories
        (append '("build"
                  "eggs"
                  "git-eggs"
                  "develop-eggs"
                  "node_modules"
                  "log"
                  "tmp")
                projectile-globally-ignored-directories))
)

(advice-add
 'projectile-relevant-known-projects
 :around
 (lambda (fn &rest args)
   (if (and (boundp 'my/projectile-force-open-projects)
            my/projectile-force-open-projects)
       (apply (symbol-function 'projectile-relevant-open-projects) args)
       (apply fn args))))

(defun my/open-project-in-new-perspective-action ()
  "Open project to magit (or any other supported VCS),
with a fallback to dired if the project is not managed with a
VCS."
  (let* ((project-root (projectile-project-root))
         (name (projectile-default-project-name project-root))
         (vc (projectile-project-vcs (projectile-project-root))))
    (persp-switch name)
    (if (eq vc 'none)
        (projectile-dired)
      (projectile-vc))))

(defun my/open-project-by-name-in-new-perspective (project-root)
  "Opens a project with the specified project root in a new perspective."
  (let ((projectile-switch-project-action 'my/open-project-in-new-perspective-action))
    (funcall 'projectile-switch-project-by-name project-root)))

(defun my/open-project-in-new-perspective ()
  "Prompts for a project to open in a new perspective."
  (interactive)
  (let ((projectile-switch-project-action 'my/open-project-in-new-perspective-action))
    (call-interactively 'projectile-switch-project)))

(defun my/open-project ()
  "Prompts for a project to open (in the current perspective)."
  (interactive)
  (projectile-switch-project))
