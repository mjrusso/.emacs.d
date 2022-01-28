(use-package projectile
  :init
  (setq projectile-use-git-grep t)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-require-project-root nil)
  ;; When using projectile commands directly, rather than the counsel variants,
  ;; use ivy completion.
  (setq projectile-completion-system 'ivy)
  :config
  (setq projectile-globally-ignored-directories
        (append '("build"
                  "eggs"
                  "git-eggs"
                  "develop-eggs"
                  "node_modules"
                  "log"
                  "tmp")
                projectile-globally-ignored-directories))
  (projectile-mode +1))

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package counsel-projectile
  :after (counsel projectile)
  :init (setq counsel-projectile-remove-current-project nil)
  :config (counsel-projectile-mode t))

(defun my/counsel-projectile-switch-open-project ()
  "A counsel version of `projectile-switch-open-project', in lieu
of one from the counsel library."
  (interactive)
  (let ((my/projectile-force-open-projects t))
    (counsel-projectile-switch-project)))

(advice-add
 'projectile-relevant-known-projects
 :around
 (lambda (fn &rest args)
   (if (and (boundp 'my/projectile-force-open-projects)
            my/projectile-force-open-projects)
       (apply (symbol-function 'projectile-relevant-open-projects) args)
       (apply fn args))))

(defun my/counsel-projectile-switch-project-action-maybe-vc (project-root)
  "Open a project to magit (or any other supported VCS), with a
fallback to dired if the project is not managed with a VCS."
  (let ((vc (projectile-project-vcs project-root)))
    (if (eq vc 'none)
        (counsel-projectile-switch-project-action-dired project-root)
      (counsel-projectile-switch-project-action-vc project-root))))

(defun my/open-project-action (project-root)
  (my/counsel-projectile-switch-project-action-maybe-vc project-root))

(defun my/open-project-in-new-tab-action (project-root)
  (tab-new)
  (my/open-project-action project-root)
  (delete-other-windows)
  (let ((name (projectile-project-name)))
    (tab-rename name)))

(defun my/open-project-prompt (open-in-new-tab)
  (ivy-read (projectile-prepend-project-name
             (if open-in-new-tab "Open project in new tab: "
               "Open project: "))
            projectile-known-projects
            :preselect (and (projectile-project-p)
                            (abbreviate-file-name (projectile-project-root)))
            :action (lambda (x)
                      (if open-in-new-tab
                          (my/open-project-in-new-tab-action x)
                        (my/open-project-action x)))
            :require-match t
            :sort counsel-projectile-sort-projects
            :caller 'my/open-project-in-new-tab))

(defun my/open-project ()
  "Prompts for a project to open. The project opens immediately
to the project's magit status buffer (or equivalent for other
VCS)."
  (interactive)
  (my/open-project-prompt nil))

(defun my/open-project-in-new-tab ()
  "Prompts for a project to open in a new tab. The project opens
immediately to the project's magit status buffer (or equivalent
for other VCS). The tab is named after the project name."
  (interactive)
  (my/open-project-prompt t))
