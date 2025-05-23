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

(defun my/file-context-list-add-directory-files (list-buffer-name)
  "Add paths of all files from a directory to LIST-BUFFER-NAME.

Uses the project-specific or default buffer name unless called with a prefix argument,
 in which case it prompts for the buffer name.

Prompts for a directory to add files from, defaulting to the project root.
Fetches all files belonging to the selected directory (respecting
ignore files via `project-files`), calculates their paths relative
to the project root. The list of files to add is sorted alphabetically.
Each unique file path is then appended to the context list buffer if not
already present.

If LIST-BUFFER-NAME does not exist, it is created and **always** displayed.
Signals an error if the current buffer is not part of a
recognized project."
  (interactive
   (list (if current-prefix-arg
             (read-buffer "Add files to context list buffer: " (my/file-context-list-buffer-name))
           (my/file-context-list-buffer-name))))

  (let* ((current-project (project-current t))
         (project-root (project-root current-project))
         (selected-dir (read-directory-name "Add files from directory: " project-root nil t)))

    (unless (file-directory-p selected-dir)
      (user-error "Not a valid directory: %s" selected-dir))

    (let* ((all-project-files (project-files current-project))
           (selected-dir-abs (expand-file-name selected-dir))
           (absolute-files (seq-filter
                            (lambda (file)
                              (string-prefix-p selected-dir-abs file))
                            all-project-files))
           (relative-files (mapcar (lambda (abs-file)
                                     (file-relative-name abs-file project-root))
                                   absolute-files))
           (sorted-relative-files (sort relative-files #'string<))
           (buffer-existed (get-buffer list-buffer-name))
           (total-to-process (length sorted-relative-files))
           (added-count 0)
           (skipped-count 0))

      (if (or (> total-to-process 0) buffer-existed)
          (let ((target-buffer (get-buffer-create list-buffer-name)))
            (with-current-buffer target-buffer
              (dolist (rel-file sorted-relative-files)
                (save-excursion
                  (goto-char (point-min))
                  (if (re-search-forward (concat "^" (regexp-quote rel-file) "$") nil t)
                      (setq skipped-count (1+ skipped-count))
                    (progn ; File not found, add it
                      (goto-char (point-max))
                      ;; Ensure a newline before inserting, if buffer is not empty and doesn't end with one
                      (unless (or (bobp) (eq (char-before (point-max)) ?\n))
                        (insert "\n"))
                      (insert rel-file)
                      ;; Ensure a newline after inserting the file name
                      (unless (eq (char-before (point-max)) ?\n)
                        (insert "\n"))
                      (setq added-count (1+ added-count)))))))

            (when (not buffer-existed)
              (pop-to-buffer target-buffer))))

      (let ((relative-selected-dir (file-relative-name selected-dir project-root)))
        (cond
         ((= total-to-process 0)
          (message "No files found in directory '%s' to add to '%s'"
                   relative-selected-dir list-buffer-name))
         ((= added-count 0)
          (message "No new files added from '%s' to '%s'. All %d files already present."
                   relative-selected-dir list-buffer-name skipped-count))
         (t
          (message "Added %d relative file paths (skipped %d already present) from '%s' to '%s'"
                   added-count
                   skipped-count
                   relative-selected-dir
                   list-buffer-name)))))))

(global-set-key (kbd "C-c a c o") #'my/file-context-list-open-buffer)
(global-set-key (kbd "C-c a c c") #'my/file-context-list-clear-buffer)
(global-set-key (kbd "C-c a c a") #'my/file-context-list-add-file)
(global-set-key (kbd "C-c a c A") #'my/file-context-list-add-directory-files)

;;; ai-file-context.el ends here
