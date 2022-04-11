(defun my/workspace:emacs-config ()
  "Get or create a workspace for editing my Emacs configuration."
  (interactive)
  (my/open-project-by-name-in-new-perspective "~/.emacs.d/")
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

(defun my/workspace:main ()
  "Get or create the main workspace.

This workspace displays information excerpted from an org file:
the first two top-level headings are displayed in their own
indirect buffers, narrowed to their subtrees, and displayed in
side windows. The third top-level heading is also displayed in an
indirect buffer, narrowed to its subtree, but displayed in a
normal window.

Note that this workspace is created automatically when Emacs
launches."
  (interactive)
  (persp-switch persp-initial-frame-name)
  (if (= 1 (count-windows))
      (let ((dashboard-file-path (concat my/primary-org-directory "dashboard.org")))
        (when (file-exists-p dashboard-file-path)
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
                (switch-to-buffer (current-buffer)))
              ))))))
