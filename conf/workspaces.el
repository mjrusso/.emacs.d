;; -*- lexical-binding: nil; -*-

(defun my/workspace:emacs-config ()
  "Get or create a workspace for editing my Emacs configuration."
  (interactive)
  (my/open-project-by-name "~/.emacs.d/")
  (if (= 1 (count-windows))
      (progn (split-window-right)
             (other-window 1)
             (let ((scratchpad-file-path (concat user-emacs-directory "scratchpad.el")))
               (when (file-exists-p scratchpad-file-path)
                 (find-file scratchpad-file-path)))
             (split-window-below)
             (other-window 1)
             (with-current-buffer (messages-buffer)
               (goto-char (point-max))
               (switch-to-buffer (current-buffer)))
             (other-window 1))))

(defun my/workspace:dashboard ()
  "Get or create the dashboard workspace."
  (interactive)
  (let ((dashboard-file-path (expand-file-name "dashboard.org" my/primary-org-directory)))
    (when (file-exists-p dashboard-file-path)
      (find-file dashboard-file-path))))
