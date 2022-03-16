(defvar my/scratch-files-dir (file-truename "~/Dropbox/scratch/")
  "Default location of scratch files")

(defun my/scratch-file ()
  "Find or create a scratch file, defaulting to the location
`my/scratch-files-dir'."
  (interactive)
  (let* ((default-prefix (format-time-string "%Y-%m-%d_"))
         (initial-path (format "%s" my/scratch-files-dir))
         (path (read-file-name "Scratch file: "
                               initial-path nil nil default-prefix)))
    (find-file path)))
