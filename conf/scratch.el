(defvar my/persistent-scratch-files-dir (file-truename "~/Dropbox/Notes/scratch/")
  "Default location of persistent scratch files")

(defun my/persistent-scratch-file ()
  "Find or create a persistent scratch file, defaulting to the
location `my/scratch-files-dir'."
  (interactive)
  (let* ((default-prefix (format-time-string "%Y-%m-%d - "))
         (initial-path (format "%s" my/persistent-scratch-files-dir))
         (path (read-file-name "Scratch file: "
                               initial-path nil nil default-prefix)))
    (find-file path)))

(defun my/find-persistent-scratch-file (name)
  "Find a persistent scratch file with the provided name located in the `my/scratch-files-dir' directory."
  (let* ((initial-path (format "%s" my/persistent-scratch-files-dir))
         (path (format "%s%s" initial-path name)))
    (find-file path)))


(defun my/daily-scratch-file ()
  "Create or open today's daily scratch file in org format."
  (interactive)
  (let* ((today (format-time-string "%Y-%m-%d"))
         (filename (format "%s.org" today))
         (filepath (expand-file-name filename my/persistent-scratch-files-dir))
         (file-exists (file-exists-p filepath)))
    (find-file filepath)
    (unless file-exists
      (insert (format "#+TITLE: Daily Notes - %s\n\n" today))
      (goto-char (point-max)))))


(global-set-key (kbd "C-c n s") 'my/persistent-scratch-file)
(global-set-key (kbd "C-c n d") 'my/daily-scratch-file)
