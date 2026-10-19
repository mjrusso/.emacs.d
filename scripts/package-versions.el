;;; package-versions.el --- Compare and apply package pins -*- lexical-binding: t; -*-

(require 'subr-x)

(defun my/package-git (directory &rest arguments)
  "Run Git in DIRECTORY with ARGUMENTS, returning its output."
  (with-temp-buffer
    (unless (zerop (apply #'process-file "git" nil '(t t) nil
                         "-C" directory arguments))
      (error "Git failed in %s: %s" directory (string-trim (buffer-string))))
    (string-trim (buffer-string))))

(defun my/package-version-differences ()
  "Return differing checkout paths paired with their lockfile commits."
  (let ((pins (with-temp-buffer
                (insert-file-contents
                 (expand-file-name "straight/versions/default.el" user-emacs-directory))
                (read (current-buffer))))
        differences)
    (dolist (pin pins (nreverse differences))
      (let ((directory (expand-file-name
                        (concat "straight/repos/" (car pin)) user-emacs-directory)))
        (when (and (file-directory-p directory)
                   (not (equal (my/package-git directory "rev-parse" "HEAD") (cdr pin))))
          (push (cons directory (cdr pin)) differences))))))

(defun my/sync-package-versions ()
  "Apply lockfile commits to existing repositories, preserving local edits.
Missing repositories are installed by Straight when the config loads."
  (let ((differences (my/package-version-differences)))
    (dolist (pin differences)
      (unless (string-empty-p (my/package-git (car pin) "status" "--porcelain"))
        (error "Local changes in %s; resolve them before syncing packages" (car pin))))
    (dolist (pin differences)
      (let ((directory (car pin))
            (commit (cdr pin)))
        (unless (zerop (process-file "git" nil nil nil "-C" directory
                                    "cat-file" "-e" (concat commit "^{commit}")))
          (message "Fetching %s at %s" (file-name-nondirectory directory) commit)
          (my/package-git directory "fetch" "origin" commit))
        (my/package-git directory "checkout" "--detach" commit)
        (message "Synced %s" (file-name-nondirectory directory))))
    (length differences)))

(defun my/warn-package-version-differences ()
  "Warn when package checkouts differ from the committed lockfile."
  (condition-case problem
      (when-let* ((differences (my/package-version-differences)))
        (display-warning
         'package-versions
         (format "Package checkouts differ from the lockfile: %s. Run `just sync-packages` in %s, then restart Emacs."
                 (mapconcat (lambda (pin) (file-name-nondirectory (car pin)))
                            differences ", ")
                 (expand-file-name user-emacs-directory))
         :warning))
    (error (display-warning 'package-versions (error-message-string problem) :warning))))
