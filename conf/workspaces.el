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

(defvar my/workspace:dashboard-buffer-names
  '("*-dashboard/sidebar-*" "*-dashboard/goals-*" "*-dashboard/notes-*")
  "Base names of the indirect buffers that make up the dashboard workspace.
Matched by prefix, so uniquified copies like `...<2>' are caught too. Must
agree with the `clone-indirect-buffer' calls in `my/workspace:dashboard'.")

(defun my/workspace:dashboard--kill-existing ()
  "Kill the indirect buffers left over from a previous dashboard.
`clone-indirect-buffer' uniquifies names, so without this a second
invocation would accumulate `...<2>' copies instead of replacing them.
Killing them also removes their dedicated side windows."
  (dolist (buffer (buffer-list))
    (when (seq-some (lambda (base) (string-prefix-p base (buffer-name buffer)))
                    my/workspace:dashboard-buffer-names)
      (kill-buffer buffer))))

(defun my/workspace:dashboard--reset-windows ()
  "Return the selected frame to a single, ordinary window.
`delete-other-windows' signals an error when the selected window is
itself a side window (`imenu-list', say), so step out of one first."
  (when (window-parameter nil 'window-side)
    (when-let* ((window (seq-find (lambda (w) (not (window-parameter w 'window-side)))
                                  (window-list))))
      (select-window window)))
  (delete-other-windows))

(defun my/workspace:dashboard ()
  "Get or create the dashboard workspace.

This workspace displays information excerpted from an org file:
the first two top-level headings are displayed in their own
indirect buffers, narrowed to their subtrees, and displayed in
side windows. The third top-level heading is also displayed in an
indirect buffer, narrowed to its subtree, but displayed in a
normal window."
  (interactive)
  (let ((dashboard-file-path (concat my/primary-org-directory "dashboard.org")))
    (when (file-exists-p dashboard-file-path)
      (my/workspace:dashboard--kill-existing)
      (my/workspace:dashboard--reset-windows)
      (let ((dashboard-buffer (find-file dashboard-file-path)))
        (with-current-buffer dashboard-buffer
          (with-current-buffer (clone-indirect-buffer "*-dashboard/sidebar-*" nil)
            (org-overview)
            (goto-char (point-min))
            (org-next-visible-heading 1)
            (org-narrow-to-subtree)
            (outline-show-all)
            (let ((display-buffer-mark-dedicated t))
              (display-buffer-in-side-window
               (current-buffer)
               '((side . left)
                 (slot . 0)
                 (window-parameters
                  (no-delete-other-windows . t))))))
          (with-current-buffer (clone-indirect-buffer "*-dashboard/goals-*" nil)
            (org-overview)
            (goto-char (point-min))
            (org-next-visible-heading 2)
            (org-narrow-to-subtree)
            (outline-show-all)
            (goto-char (point-max))
            (let ((display-buffer-mark-dedicated t))
              (display-buffer-in-side-window
               (current-buffer)
               '((side . left)
                 (slot . 1)
                 (window-parameters
                  (no-delete-other-windows . t))))))
          (with-current-buffer (clone-indirect-buffer "*-dashboard/notes-*" nil)
            (org-overview)
            (org-hide-drawer-all)
            (goto-char (point-min))
            (org-next-visible-heading 3)
            (org-narrow-to-subtree)
            (org-reveal)
            (org-cycle)
            (switch-to-buffer (current-buffer))))))))
