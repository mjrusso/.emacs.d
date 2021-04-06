(require 'projectile)

(projectile-global-mode)

(setq projectile-use-git-grep t)

(setq projectile-enable-caching t)

(setq projectile-indexing-method 'alien)

(setq projectile-require-project-root nil)

(setq projectile-globally-ignored-directories
      (append '("build"
                "eggs"
                "git-eggs"
                "develop-eggs"
                "node_modules"
                "log"
                "tmp")
              projectile-globally-ignored-directories))

(require 'counsel-projectile)

(counsel-projectile-mode t)

(setq counsel-projectile-remove-current-project nil)

;; When using projectile commands directly, rather than the counsel variants,
;; use ivy completion.
(setq projectile-completion-system 'ivy)
