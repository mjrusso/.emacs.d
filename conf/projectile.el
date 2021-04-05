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
