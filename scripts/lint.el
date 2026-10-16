;;; lint.el --- Check repository Emacs Lisp syntax -*- lexical-binding: t; -*-

(require 'lisp-mode)

(let* ((default-directory
         (file-name-as-directory
          (expand-file-name ".." (file-name-directory load-file-name))))
       (files (with-temp-buffer
                (unless (zerop (process-file "git" nil t nil "ls-files" "-z"
                                             "--cached" "--others" "--exclude-standard"
                                             "--" "*.el"))
                  (error "Cannot list repository Lisp files"))
                (split-string (buffer-string) "\0" t)))
       (checked 0)
       (failures 0))
  (dolist (file files)
    (when (file-exists-p file)
      (setq checked (1+ checked))
      (with-temp-buffer
        (insert-file-contents file)
        (emacs-lisp-mode)
        (condition-case problem
            (progn
              (check-parens)
              (goto-char (point-min))
              (while (progn (forward-comment (point-max)) (not (eobp)))
                (read (current-buffer))))
          (error
           (setq failures (1+ failures))
           (princ (format "%s:%d: %s\n" file (line-number-at-pos)
                          (error-message-string problem))))))))
  (princ (format "Checked %d Lisp files; %d syntax errors.\n" checked failures))
  (kill-emacs (if (zerop failures) 0 1)))
