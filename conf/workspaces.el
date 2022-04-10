(defun my/workspace-emacs-config ()
  "Get or create a workspace for editing my .emacs.d configuration."
  (interactive)
  (my/open-project-by-name-in-new-perspective "~/.emacs.d/")
  (if (= 1 (count-windows))
      (progn (split-window-right)
             (other-window 1)
             (find-file (concat user-emacs-directory "scratchpad.el"))
             (split-window-below)
             (other-window 1)
             (with-current-buffer (messages-buffer)
               (goto-char (point-max))
               (switch-to-buffer (current-buffer)))
             (other-window 1))))

(defun my/workspace-main ()
  "Get or create the main workspace, which displays a \"dashboard\" of useful information."
  (interactive)
  (persp-switch persp-initial-frame-name)
  (if (= 1 (count-windows))
      (progn
        (let ((dashboard-buffer (find-file (concat my/primary-org-directory "dashboard.org"))))
          (with-current-buffer dashboard-buffer
            (org-overview)
            (org-hide-drawer-all)
            (goto-char (point-min))
            (org-next-visible-heading 3)
            (org-reveal)
            (org-cycle)
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
                      (no-delete-other-windows . t)))))))
            (with-current-buffer (clone-indirect-buffer "*-dashboard/main-*" nil)
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
                    (no-delete-other-windows . t)))))))
          ))))


(defun my/workspace-main-2 ()
  "Get or create the main workspace, which is displayed automatically when emacs launches."
  (interactive)
  (persp-switch persp-initial-frame-name)
  (if (= 1 (count-windows))
      (progn
        (let ((dashboard-buffer (find-file (concat my/primary-org-directory "dashboard.org"))))
          (with-current-buffer dashboard-buffer
            (with-current-buffer (clone-indirect-buffer "*-dashboard/main-*" nil)
              (goto-char (point-min))
              (org-next-visible-heading 2)
              (org-narrow-to-subtree)
              (outline-show-all)
              (goto-char (point-max))
              (switch-to-buffer (current-buffer))))
          (with-current-buffer dashboard-buffer
            (with-current-buffer (clone-indirect-buffer "*-dashboard/sidebar-*" nil)
              (goto-char (point-min))
              (org-next-visible-heading 1)
              (org-narrow-to-subtree)
              (outline-show-all)
              (let ((display-buffer-mark-dedicated t))
                (display-buffer-in-side-window
                 (current-buffer)
                 '((side . left)
                   (window-parameters
                    (no-delete-other-windows . t)))))))))))


(defun my/workspace-main-3 ()
  "Get or create the main workspace, which is displayed automatically when emacs launches."
  (interactive)
  (persp-switch persp-initial-frame-name)
  (if (= 1 (count-windows))
      (progn
        (let ((dashboard-buffer (find-file (concat my/primary-org-directory "dashboard.org"))))
          (with-current-buffer dashboard-buffer
            (with-current-buffer (clone-indirect-buffer "*-dashboard/main-*" nil)
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
                    (no-delete-other-windows . t)))))))
                                        ;              (switch-to-buffer (current-buffer))))
          (with-current-buffer dashboard-buffer
            (with-current-buffer (clone-indirect-buffer "*-dashboard/sidebar-*" nil)
              (goto-char (point-min))
              (org-next-visible-heading 1)
              (org-narrow-to-subtree)
              (outline-show-all)
              (let ((display-buffer-mark-dedicated t))
                (display-buffer-in-side-window
                 (current-buffer)
                 '((side . left)
                   (window-parameters
                    (no-delete-other-windows . t)))))))))))
