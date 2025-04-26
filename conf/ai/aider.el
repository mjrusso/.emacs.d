(defun my/aider-copy-context (list-buffer-name)
  "Run aider on files listed in LIST-BUFFER-NAME asynchronously.

Suggests the current project's list buffer by default. Runs the
command with the current project's root directory as the working
directory. Sends asynchronous output to a project-specific buffer
like '*project-name-aider-output*'. Requires being in a project
context if the list buffer contains relative paths."

  (interactive
   (list (read-buffer "Buffer containing file context list: "
                      (my/get-file-context-list-buffer-name))))

  (let ((buffer (get-buffer list-buffer-name)))
    (unless buffer
      (error "Buffer '%s' does not exist" list-buffer-name))

    (let* ((current-project (project-current))
           (project-root (and current-project (project-root current-project)))
           (output-buffer-name
            (if project-root
                (format "*%s-aider-output*"
                        (file-name-nondirectory (directory-file-name project-root)))
              "*aider-output*"))
           (buffer-scope (if project-root "project" "global"))
           (command-cwd (or project-root default-directory)))

      (with-current-buffer buffer
        (let* ((files (split-string (buffer-string) "\n+" t "[ \t]+")))

          (if (null files)
              (message "No files found in buffer '%s'. Aider command not run." list-buffer-name)

            ;; Filter out comment lines (# or //, potentially with leading whitespace)
            (let* ((filtered-files (seq-filter (lambda (line)
                                                 (not (string-match-p "^[ \t]*\\(//\\|#\\)" line)))
                                               files)))

              ;; Prepare file arguments (resolve relative paths using project-root if available)
              (let* ((files-for-aider ; Renamed from absolute-files for clarity
                      (mapcar (lambda (f)
                                (cond
                                 ;; If path is already absolute, use it directly
                                 ((file-name-absolute-p f) f)
                                 ;; If path is relative AND we have a project root, make it absolute based on project root
                                 (project-root f)

                                 ;; Otherwise (relative path but no project root), signal error
                                 (t (error "Relative path '%s' found in list but not in a project context" f))))
                              filtered-files))

                     (quoted-files (mapcar #'shell-quote-argument files-for-aider))
                     (files-arg-string (mapconcat #'identity quoted-files " "))
                     (command (format "aider %s --message \"/copy-context\""
                                      files-arg-string)))

                ;; --- Execute Asynchronously with Specific Output Buffer and CWD ---
                (message "Running asynchronously in %s context (CWD: %s): %s (Output -> %s)"
                         buffer-scope command-cwd command output-buffer-name)

                ;; Use LET to bind default-directory *specifically* for the async call
                (let ((default-directory command-cwd))
                  (async-shell-command command output-buffer-name))
                (display-buffer output-buffer-name) ; Display the output buffer immediately
                ))))))))


(defun my/aider-copy-repo-map ()
  "Copy the Aider repo map for the current project by running the `aider' command-line tool asynchronously.

Runs the command with the current project's root directory as the
working directory if applicable. Sends asynchronous output to a
project-specific buffer like '*project-name-aider-output*' or
'*aider-output*' otherwise."
  (interactive)
  (let* ((current-project (project-current))
         (project-root (and current-project (project-root current-project)))
         (output-buffer-name
          (if project-root
              (format "*%s-aider-output*"
                      (file-name-nondirectory (directory-file-name project-root)))
            "*aider-output*"))
         (buffer-scope (if project-root "project" "global"))
         (command-cwd (or project-root default-directory))
         (command "aider --message \"/copy-context\"")) ; The specific command

    (message "Running asynchronously in %s context (CWD: %s): %s (Output -> %s)"
             buffer-scope command-cwd command output-buffer-name)
    (let ((default-directory command-cwd))
      (async-shell-command command output-buffer-name))
    (display-buffer output-buffer-name)))
