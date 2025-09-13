(require 'cl-lib)
(require 'subr-x)

(defvar my/project-sentinels
  '("mix.exs"          ; Elixir
    "package.json"     ; Node/JS/TS
    "Cargo.toml"       ; Rust
    "go.mod"           ; Go
    "pyproject.toml"   ; Python
    "Gemfile"          ; Ruby
    "deps.edn"         ; Clojure
    "project.clj"      ; Clojure
    "shadow-cljs.edn"  ; ClojureScript
    "build.gradle"     ; Java/Kotlin
    "build.gradle.kts" ; Kotlin
    "pom.xml"          ; Java/Maven
    "composer.json"    ; PHP
    "stack.yaml"       ; Haskell
    "cabal.project"    ; Haskell
    )
  "File names that mark sub-project roots in a monorepo.")

;; Properly support monorepos when using eglot.
;;
;; By default, eglot assumes that the project root is the repository root.
;; However, in a monorepo, this is generally not correct: the language server
;; will typically need (or want) to be started in a specific subfolder.
;;
;; An alternative approach is to customize the `project-vc-extra-root-markers'
;; variable, but that change affects all project-related commands. (Instead,
;; `eglot-lsp-context' only affects how an LSP server is started by eglot.)

;; For additional background, see:
;; https://gist.github.com/pesterhazy/e8e445e6715f5d8bae3c62bc9db32469
(defun my/eglot-monorepo-project-finder (dir)
  (when (bound-and-true-p eglot-lsp-context)
    (when-let ((project-dir
                (locate-dominating-file
                 dir
                 (lambda (directory)
                   (cl-some (lambda (sentinel)
                              (file-exists-p (expand-file-name sentinel directory)))
                            my/project-sentinels)))))
      (condition-case nil
          (list 'vc (vc-responsible-backend project-dir) project-dir)
        (error (cons 'transient project-dir))))))

;; https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el
(use-package project
  :ensure nil

  :config
  (define-key global-map (kbd "C-c p") project-prefix-map)
  (add-hook 'project-find-functions #'my/eglot-monorepo-project-finder)

  :init
  (defun my/project-switch-project (&optional project-root)
    "Switch to a project, optionally specifying PROJECT-ROOT directly."
    (interactive)
    (let* ((project (or project-root (project-prompt-project-dir)))
           (default-directory project)
           (proj (project-current))
           (project-name (file-name-nondirectory (directory-file-name project)))
           (file-context-list-buffer-name (my/file-context-list-buffer-name))
           (file-context-list-buffer (get-buffer-create file-context-list-buffer-name)))
      (when project
        ;; This setup is optimized for TTY Emacs, where we simply start new
        ;; emacsclient instances for each project/context. (Each emacsclient
        ;; instance automatically gets a new frame, so `beframe' will
        ;; automatically provide frame-level isolation. As a hack, we also
        ;; create a new frame when switching projects, because this prevents
        ;; `beframe' from assuming the most-recently visible buffer, which may
        ;; not be part of the new project.)
        ;;
        ;; See additional discussion in the comments on this commit:
        ;; https://github.com/mjrusso/.emacs.d/commit/2399bdbfb166a96e46d0bd8854eac2d7e562957e
        ;;        (set-frame-name project-name)
        (delete-other-windows)
        (if (eq (car proj) 'vc)
            (magit-status)
          (project-dired))
        (make-frame `((name . ,(format "%s" project-name))))
        (my/beframe-assume-buffer file-context-list-buffer))))

  (defun my/is-project-directory-p (directory)
    "Check if DIRECTORY is a project directory using project.el.
Returns t if directory is recognized as a project, nil otherwise."
    (when (and directory (file-directory-p directory))
      (let ((default-directory (expand-file-name directory)))
	(when-let* ((project (project-current nil)))
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
