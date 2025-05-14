;;; ai-file-context.el --- Helper functions to maintain context for AI tooling -*- lexical-binding: t -*-

;;; Commentary:
;;
;; `ai-file-context' provides functionality for maintaining a list of files
;; (via special buffers). This is helpful when used in conjunction with AI
;; tooling, such as Aider.

;;; Code:

(require 'project)

(defun my/file-context-list-buffer-name ()
  "Return the project-specific file list buffer name.
Returns the name based on the current project's root directory name.
If not inside a project, returns the default (fallback) value."
  (let* ((current-project (project-current))
         (project-root (and current-project (project-root current-project))))
    (if project-root
        (format "*%s-file-context-list*"
                (file-name-nondirectory (directory-file-name project-root)))
      "*file-context-list*")))

(defun my/file-context-list-open-buffer (list-buffer-name)
  "Display the file context list buffer with name LIST-BUFFER-NAME.

Uses the project-specific or default buffer name unless called with a prefix argument,
in which case it prompts for the buffer name, defaulting to the current project's
list buffer or the global default. Creates the buffer if it doesn't exist."
  (interactive
   (list (if current-prefix-arg
             (read-buffer "Buffer to display file context list: " (my/file-context-list-buffer-name))
           (my/file-context-list-buffer-name))))
  (pop-to-buffer list-buffer-name)
  (message "Displayed file list buffer: %s" list-buffer-name))

(defun my/file-context-list-clear-buffer (list-buffer-name)
  "Clear the file context list buffer LIST-BUFFER-NAME.

Uses the project-specific or default buffer name unless called with a prefix argument,
in which case it prompts for the buffer name."
  (interactive
   (list (if current-prefix-arg
             (read-buffer "File context list buffer to clear: " (my/file-context-list-buffer-name))
           (my/file-context-list-buffer-name))))
  (let ((buffer (get-buffer list-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (erase-buffer)
          (message "Cleared buffer: %s" list-buffer-name))
      (message "Buffer '%s' does not exist." list-buffer-name))))

(defun my/file-context-list-add-file (list-buffer-name)
  "Add current buffer's file name to LIST-BUFFER-NAME.

Uses the project-specific or default buffer name unless called with a prefix argument,
in which case it prompts for the buffer name.

If the file is within the project, adds the relative path.
Otherwise, adds the absolute path.
If LIST-BUFFER-NAME does not exist, it is created and **always** displayed.
Prevents adding the list buffer itself or other context list buffers.
An error is signaled if the current buffer is not visiting a file,
or if it matches the pattern for a context list buffer."
  (interactive
   (list (if current-prefix-arg
             (read-buffer "Add file to context list buffer: " (my/file-context-list-buffer-name))
           (my/file-context-list-buffer-name))))


  (let ((current-buffer-name (buffer-name (current-buffer)))
        (absolute-file-name (buffer-file-name (current-buffer))))

    (cond
     ((string= current-buffer-name list-buffer-name)
      (user-error "Cannot add the target list buffer '%s' to itself" list-buffer-name))

     ((string-match-p "-file-context-list\\*$\\|\\*file-context-list\\*" current-buffer-name)
      (user-error "Cannot add buffer '%s' as it looks like a context list buffer" current-buffer-name))

     ((null absolute-file-name)
      (user-error "Current buffer '%s' has no associated file" current-buffer-name))

     (t
      (save-current-buffer
        (save-excursion
          (let* ((current-project (project-current))
                 (project-root (and current-project (project-root current-project)))
                 (name-to-insert (if project-root
                                     (file-relative-name absolute-file-name project-root)
                                   absolute-file-name))
                 (path-type (if project-root "relative" "absolute"))
                 (buffer-existed (get-buffer list-buffer-name))
                 (target-buffer (get-buffer-create list-buffer-name)))

            (with-current-buffer target-buffer
              (save-excursion
                (goto-char (point-min))
                (if (re-search-forward (concat "^" (regexp-quote name-to-insert) "$") nil t)
                    (message "'%s' already exists in '%s'" name-to-insert list-buffer-name)
                  (goto-char (point-max))
                  (unless (or (bobp) ; Buffer is empty
                              (eq (char-before (point-max)) ?\n))
                    (insert "\n"))
                  (insert name-to-insert)
                  (unless (eq (char-before (point-max)) ?\n)
                    (insert "\n"))
                  (message "Added %s path '%s' to '%s'" path-type name-to-insert list-buffer-name)
                  )))

            (when (not buffer-existed)
              (pop-to-buffer target-buffer))

            )))))))

(defun my/file-context-list-add-directory-files ()
  "Add paths of all files from a directory to the context list, sorted.

Prompts for a directory to add files from, defaulting to the project root.
Fetches all files belonging to the selected directory (respecting
ignore files via `project-files`), calculates their paths relative
to the project root, sorts them alphabetically, and inserts the sorted
relative paths into the context list buffer.

The target buffer is determined by `my/file-context-list-buffer-name`.
Signals an error if the current buffer is not part of a
recognized project."
  (interactive)
  (let* ((current-project (project-current t))
         (project-root (project-root current-project))
         (selected-dir (read-directory-name "Add files from directory: " project-root nil t))
         (list-buffer-name (my/file-context-list-buffer-name)))

    (unless (file-directory-p selected-dir)
      (user-error "Not a valid directory: %s" selected-dir))

    (let* ((absolute-files (directory-files-recursively selected-dir ".*" nil t))
           (relative-files (mapcar (lambda (abs)
                                     (file-relative-name abs project-root))
                                   absolute-files))
           (sorted-relative-files (sort relative-files #'string<))
           (file-count (length sorted-relative-files))
           (target-buffer (get-buffer-create list-buffer-name)))

      (with-current-buffer target-buffer
        (let ((buffer-read-only nil))
          (erase-buffer)
          (dolist (rel-file sorted-relative-files)
            (insert rel-file "\n"))))

      (message "Wrote %d files from '%s' to '%s'"
               file-count
               (file-relative-name selected-dir project-root)
               list-buffer-name)
      (pop-to-buffer target-buffer))))

(global-set-key (kbd "C-c a c o") #'my/file-context-list-open-buffer)
(global-set-key (kbd "C-c a c c") #'my/file-context-list-clear-buffer)
(global-set-key (kbd "C-c a c a") #'my/file-context-list-add-file)
(global-set-key (kbd "C-c a c A") #'my/file-context-list-add-directory-files)

;;; ai-file-context.el ends here
