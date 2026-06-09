;; Ghostel: Emacs terminal emulator powered by libghostty-vt.  -*- lexical-binding: t; -*-
;;
;; Details: https://github.com/dakra/ghostel

;; Forward-declare the package's special variables that the helpers below
;; dynamically `let'-bind (or `setq').  Ghostel is loaded lazily, so its
;; `defcustom's haven't run when this file is read; without these declarations
;; `lexical-binding' would treat the `let' bindings as lexical locals, so the
;; rebindings would never reach `ghostel'.
(defvar ghostel-buffer-name)
(defvar ghostel-shell)
(defvar ghostel--managed-buffer-name)

(use-package ghostel
  :straight (ghostel
             :type git
             :host github
             :repo "dakra/ghostel"
             :files (:defaults "etc" "src" "vendor"
                      "build.zig" "build.zig.zon" "symbols.map"))

  :preface

  (defun my/ghostel-pin-buffer-name (buffer)
    "Stop BUFFER from auto-renaming itself to follow terminal titles.
Returns BUFFER. The named-terminal commands below choose deliberate
buffer names; without this, the first title the shell reports (OSC 2)
would clobber that name via `ghostel-set-title-function'. Set
buffer-locally and synchronously, before the async output filter that
processes the title runs. (`my/ghostel-exec' deliberately does the
opposite: it wants title-following.)"
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq-local ghostel-set-title-function nil)))
    buffer)

  (defun my/named-ghostel-for-current-project (name)
    "Switch to the project-specific ghostel terminal buffer identified
by NAME if it already exists, or create a new ghostel terminal
buffer named after the current project and the provided NAME."
    (let* ((project (project-current))
           (project-root (project-root project))
           (project-name (file-name-nondirectory (directory-file-name project-root)))
           (ghostel-buffer-name (format "*ghostel-%s[%s]*" name project-name))
           (default-directory project-root))
      (my/ghostel-pin-buffer-name (ghostel))))

  (defun my/new-ghostel-for-current-project ()
    "Open a new ghostel terminal named after the current project.
Unlike `my/named-ghostel-for-current-project', this command always
opens a brand-new terminal (i.e., it does not switch to the project
specific term buffer if it already exists)."
    (interactive)
    (let* ((project (project-current))
           (project-root (project-root project))
           (project-name (file-name-nondirectory (directory-file-name project-root)))
           (ghostel-buffer-name (format "*ghostel %s*" project-name))
           (default-directory project-root))
      (my/ghostel-pin-buffer-name (ghostel '(4)))))

  (defun my/new-named-ghostel (term-name)
    "Open a ghostel terminal with buffer name TERM-NAME."
    (interactive "sTerminal name: ")
    (let ((ghostel-buffer-name (format "*ghostel %s*" term-name)))
      (my/ghostel-pin-buffer-name (ghostel '(4)))))

  (defun my/new-named-ghostel-other-window (term-name)
    "Open a ghostel terminal with buffer name TERM-NAME in the other
window."
    (interactive "sTerminal name: ")
    (let ((ghostel-buffer-name (format "*ghostel %s*" term-name))
          (display-buffer-overriding-action '((display-buffer-pop-up-window))))
      (my/ghostel-pin-buffer-name (ghostel '(4)))))

  (defun my/ghostel-exec (program &optional args)
    "Run PROGRAM with ARGS in a fresh ghostel buffer named after PROGRAM.

Intended for long-running, full-screen programs (TUIs, REPLs, AI
agents): the program is launched via a shell that first clears the
screen, working around an issue with `ghostel-exec' where full-screen
programs render with artifacts until the Emacs window is resized. The
buffer also auto-renames to follow the program's terminal title.

For short-lived commands whose output you want to read, use
`ghostel-compile' (a TTY-backed `compile' with `next-error' support) or
plain \\[shell-command] / \\[async-shell-command]."
    (declare (indent 1))
    (interactive
     (let ((parts (split-string-and-unquote
                   (read-shell-command "Run in ghostel: "))))
       (list (car parts) (cdr parts))))
    (unless (and program (not (string-empty-p program)))
      (user-error "No program given"))
    ;; Ensure the package is loaded, otherwise let-binding `ghostel-shell'
    ;; before the package is loaded is an error.
    (require 'ghostel)
    (let* ((cmd (concat "printf '\\033[H\\033[2J'; exec" ; move cursor home and
                                        ; clear the screen
                        " "
                        (shell-quote-argument program)
                        " "
                        (mapconcat #'shell-quote-argument args " ")))
           (ghostel-shell (list "/bin/sh" "-c" cmd))
           (ghostel-buffer-name (concat "*" program "*"))
           (buffer (ghostel t)))
      (with-current-buffer buffer
        (setq-local ghostel-kill-buffer-on-exit t)
        (setq-local ghostel-set-title-function
                    (lambda (title)
                      (setq ghostel--managed-buffer-name nil)
                      (rename-buffer (format "*%s: %s*" program title) t))))
      buffer))

  (defun my/ghostel-exec-project (program &optional args)
    "Run PROGRAM with ARGS via `my/ghostel-exec' from the project root.
Interactively, prompt for a shell command line and split it into
PROGRAM and ARGS."
    (declare (indent 1))
    (interactive
     (let ((parts (split-string-and-unquote
                   (read-shell-command "Run in project ghostel: "))))
       (list (car parts) (cdr parts))))
    (let* ((project (project-current t))
           (default-directory (project-root project)))
      (my/ghostel-exec program args)))

  ;; Route programs spawned in a ghostel terminal that need an editor
  ;; (e.g. `git commit', `git rebase -i', anything using $EDITOR) to the
  ;; running Emacs via `with-editor'/emacsclient, instead of launching a
  ;; nested editor. See the `ghostel-pre-spawn-hook' wiring below.
  (defun my/ghostel-pre-spawn-with-editor (&rest _args)
    "Inject `with-editor' env vars into the about-to-spawn ghostel process."
    (require 'with-editor)
    (let (with-editor-env)
      ;; `with-editor' sets values on `process-environment'.  Give it a
      ;; blank `process-environment' and use that to extract the values it
      ;; sets.
      (let (process-environment)
        (with-editor
          (setq with-editor-env (copy-sequence process-environment))))
      ;; Add the extracted values to the actual `process-environment'.
      ;;
      ;; We don't use `ghostel-environment' because `process-environment'
      ;; values take precedence.  EDITOR, etc. will already be set on
      ;; `process-environment' via `envrc', so we need to clobber these.
      (dolist (e with-editor-env)
        (add-to-list 'process-environment e))))

  ;; `C-c t' is a prefix for project-scoped ghostel terminals.
  :bind
  (:prefix-map my/ghostel-prefix-map
               :prefix "C-c t"
               ("a" . (lambda () (interactive) (my/named-ghostel-for-current-project "primary")))
               ("s" . (lambda () (interactive) (my/named-ghostel-for-current-project "secondary")))
               ("d" . (lambda () (interactive) (my/named-ghostel-for-current-project "tertiary")))
               ("f" . (lambda () (interactive) (my/named-ghostel-for-current-project "quaternary")))
               ("g" . my/new-ghostel-for-current-project)
               ("r" . my/ghostel-exec-project)   ; long-running / TUI programs
               ("c" . ghostel-compile))          ; short-lived commands, with output

  :hook
  (ghostel-pre-spawn . my/ghostel-pre-spawn-with-editor)
  (ghostel-mode . (lambda () (setq-local show-trailing-whitespace nil))))
