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
(declare-function ghostel-compile "ghostel-compile")

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
buffer named after the current project and the provided NAME.

When the current buffer is not in a project, fall back to the
buffer's own `default-directory' (i.e. the folder of the visited
file) and name the terminal after that directory instead.  If the
buffer has no `default-directory' either, fall back to the home
directory."
    (let* ((project (project-current))
           (root (or (and project (project-root project)) default-directory "~/"))
           (root-name (file-name-nondirectory (directory-file-name root)))
           (ghostel-buffer-name (format "*ghostel-%s[%s]*" name root-name))
           (default-directory root))
      (my/ghostel-pin-buffer-name (ghostel))))

  (defun my/new-ghostel-for-current-project ()
    "Open a new ghostel terminal named after the current project.
Unlike `my/named-ghostel-for-current-project', this command always
opens a brand-new terminal (i.e., it does not switch to the project
specific term buffer if it already exists)."
    (interactive)
    (let* ((project (project-current))
           (root (or (and project (project-root project)) default-directory "~/"))
           (root-name (file-name-nondirectory (directory-file-name root)))
           (ghostel-buffer-name (format "*ghostel %s*" root-name))
           (default-directory root))
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

  (defun my/ghostel-command-shell (command)
    "Return a `ghostel-shell' value that runs COMMAND through `ghostel-shell'.
Use a login command shell so shell-local setup, including autoloaded
functions, is available."
    (let ((shell (if (listp ghostel-shell)
                     ghostel-shell
                   (list ghostel-shell))))
      (append shell (list "-lc" command))))

  (defun my/ghostel-command-name (program args)
    "Return a display name for PROGRAM and ARGS."
    (mapconcat #'identity (cons program args) " "))

  (defun my/ghostel-project-root-or-default ()
    "Return the directory ghostel project commands should default to.
In a monorepo, prefer the nearest sub-project root at or above the
current `default-directory' (via `my/project-monorepo-root'); otherwise
use the current project root, falling back to `default-directory' when
there is no project."
    (if-let* ((project (project-current)))
        (let ((project-root (project-root project)))
          (or (and (fboundp 'my/project-monorepo-root)
                   (my/project-monorepo-root default-directory project-root))
              project-root))
      default-directory))

  (defun my/ghostel-read-directory (default-dir)
    "Read an existing working directory, defaulting to DEFAULT-DIR.
The minibuffer can be used to navigate up or down a level."
    (read-directory-name "Working directory: " default-dir default-dir t))

  (defun my/ghostel-exec (program &optional args directory)
    "Run PROGRAM with ARGS in a fresh ghostel buffer named after PROGRAM.

When DIRECTORY is non-nil, run from there; otherwise use the current
`default-directory'.  Interactively, a prefix argument (\\[universal-argument])
prompts for the working directory.

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
                   (read-shell-command "Run in ghostel: ")))
           (directory (when current-prefix-arg
                        (my/ghostel-read-directory default-directory))))
       (list (car parts) (cdr parts) directory)))
    (unless (and program (not (string-empty-p program)))
      (user-error "No program given"))
    ;; Ensure the package is loaded, otherwise let-binding `ghostel-shell'
    ;; before the package is loaded is an error.
    (require 'ghostel)
    (let* ((default-directory (if directory
                                  (expand-file-name directory)
                                default-directory))
           (command-name (my/ghostel-command-name program args))
           (cmd (concat "printf '\\033[H\\033[2J';" ; move cursor home, clear screen
                        " "
                        (shell-quote-argument program)
                        " "
                        (mapconcat #'shell-quote-argument args " ")))
           (ghostel-shell (my/ghostel-command-shell cmd))
           (ghostel-buffer-name (format "*%s*" command-name))
           (buffer (ghostel t)))
      (with-current-buffer buffer
        (setq-local ghostel-kill-buffer-on-exit nil)
        (setq-local ghostel-set-title-function
                    (lambda (title)
                      (setq ghostel--managed-buffer-name nil)
                      (rename-buffer (format "*%s: %s*" command-name title) t))))
      buffer))

  (defun my/ghostel-exec-project (program &optional args directory)
    "Run PROGRAM with ARGS via `my/ghostel-exec' from DIRECTORY.
Interactively, prompt for a shell command line and split it into
PROGRAM and ARGS.  DIRECTORY defaults to the current project root (as
found by the monorepo-aware project finder); with a prefix argument
(\\[universal-argument]), prompt for it instead, using the minibuffer to
navigate up or down a level."
    (declare (indent 1))
    (interactive
     (let ((parts (split-string-and-unquote
                   (read-shell-command "Run in project ghostel: ")))
           (directory (when current-prefix-arg
                        (my/ghostel-read-directory
                         (my/ghostel-project-root-or-default)))))
       (list (car parts) (cdr parts) directory)))
    (my/ghostel-exec program args
      (or directory (my/ghostel-project-root-or-default))))

  (defvar my/ghostel-exec-program-options
    '("llm-agents codex"
      "llm-agents codex --yolo"
      "llm-agents claude-code"
      "llm-agents claude-code --dangerously-skip-permissions")
    "Preset command lines offered by `my/ghostel-exec-select'.")

  (defun my/ghostel-exec-program-table (string predicate action)
    "Completion table over `my/ghostel-exec-program-options'.
Advertises an `identity' `display-sort-function' so the completion UI
(e.g. Vertico) presents the presets in `defvar' order rather than
re-sorting them."
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity))
      (complete-with-action action my/ghostel-exec-program-options
                            string predicate)))

  (defun my/ghostel-exec-select (command directory)
    "Run COMMAND via `my/ghostel-exec' from DIRECTORY.

Interactively, offer the preset command lines in
`my/ghostel-exec-program-options' to select from (or type a custom
command line), then prompt for the working directory.  The directory
defaults to the current project root (as found by the monorepo-aware
project finder); use the minibuffer to navigate up or down a level."
    (declare (indent 1))
    (interactive
     (let* ((command (completing-read
                      "Run in ghostel: "
                      #'my/ghostel-exec-program-table
                      nil nil nil nil
                      (car my/ghostel-exec-program-options)))
            (directory (my/ghostel-read-directory
                        (my/ghostel-project-root-or-default))))
       (list command directory)))
    (let ((parts (split-string-and-unquote command)))
      (my/ghostel-exec (car parts) (cdr parts) directory)))

  (defun my/ghostel-compile-project (command &optional directory)
    "Run COMMAND via `ghostel-compile' from DIRECTORY.
Use this for short-lived commands whose output you want to read: output
appears in a `compilation-mode'-style buffer that scrolls as it arrives
and supports `next-error', rather than the screen-clearing,
title-following terminal that `my/ghostel-exec' sets up for full-screen
TUIs (where short output ends up scrolled off the top of a tall grid).

Interactively, prompt for a shell command.  DIRECTORY defaults to the
current project root (as found by the monorepo-aware project finder);
with a prefix argument (\\[universal-argument]), prompt for it instead,
using the minibuffer to navigate up or down a level."
    (declare (indent 1))
    (interactive
     (let ((command (read-shell-command "Ghostel compile: "))
           (directory (when current-prefix-arg
                        (my/ghostel-read-directory
                         (my/ghostel-project-root-or-default)))))
       (list command directory)))
    (let ((default-directory (expand-file-name
                              (or directory
                                  (my/ghostel-project-root-or-default)))))
      (ghostel-compile command)))

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
               ("r" . my/ghostel-exec-project)      ; long-running / TUI programs
               ("e" . my/ghostel-exec-select)       ; pick a preset program + working dir
               ("c" . my/ghostel-compile-project))  ; short-lived commands, with output

  :hook
  (ghostel-pre-spawn . my/ghostel-pre-spawn-with-editor)
  (ghostel-mode . (lambda () (setq-local show-trailing-whitespace nil))))
