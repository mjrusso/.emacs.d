(use-package org

  :bind

  (:map org-mode-map
        (("C-c i d" . org-time-stamp-inactive)
         ("C-c i t" . org-insert-structure-template)))

  :config

  (setq
   org-adapt-indentation nil
   org-indent-indentation-per-level 0
   org-startup-folded t
   org-startup-indented nil
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"
   org-use-fast-todo-selection 'expert ; https://emacs.stackexchange.com/a/69927
   )

  (setq org-todo-keywords '((sequence "TODO(t)" "HOLD(h)" "NOTE(n)" "|" "DONE(d)" "DEAD(x)")))

  (setq org-startup-with-inline-images t)

  ;; https://www.reddit.com/r/orgmode/comments/l215r5/terrible_performance_with_inline_images_on_macs/
  ;; https://www.reddit.com/r/emacs/comments/55zk2d/adjust_the_size_of_pictures_to_be_shown_inside/d8geca2
  ;; (setq org-image-actual-width (/ (display-pixel-width) 3))
  ;; https://www.reddit.com/r/orgmode/comments/i6hl8b/comment/g1vsef2/
  (setq org-image-actual-width (/ (window-pixel-width) 3))

  ;; https://www.reddit.com/r/emacs/comments/mt51h4/strike_through_orgdone_tasks_in_agenda_to/
  (set-face-attribute 'org-headline-done nil :strike-through t)

  :init

  ;; Use visual line mode by default.
  (add-hook 'org-mode-hook 'turn-off-auto-fill)
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)

  )


;; https://github.com/minad/org-modern
(use-package org-modern
  :disabled

  :custom
  (org-modern-star nil)
  (org-modern-todo nil)
  (org-modern-hide-stars nil)
  (org-modern-timestamp nil)
  (org-modern-variable-pitch nil)

  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  )

;; Adapted from https://zzamboni.org/post/my-emacs-configuration-with-commentary/
(use-package org-agenda
  :straight nil
  :after org
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-files (list "~/Dropbox/org/"))
  (org-agenda-include-diary t)
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              (todo . " %i %-12:c%l") ; indent by level to show nesting
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  )



;; https://github.com/abo-abo/org-download
;; https://www.orgroam.com/manual.html#Org_002ddownload
;; https://zzamboni.org/post/how-to-insert-screenshots-in-org-documents-on-macos/
(use-package org-download
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-screenshot-method "pngpaste %s")
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-clipboard)
         ("s-y" . org-download-yank))))

;; Org-roam uses ripgrep (when it's installed) for better performance.
;;
;; ripgrep: https://github.com/BurntSushi/ripgrep

;; To update an existing org-mode document to work with org-roam, run the
;; command `M-x org-id-get-create` from a corresponding buffer. This will add
;; the requisite :ID: needed for org-roam to find the file. For new documents,
;; simply run `org-roam-node-file`.

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory (file-truename "~/Dropbox/org/"))
  (org-roam-db-autosync-mode)
  :custom
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "${slug}.org"
                         "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)))
  :bind (("C-c n f" . org-roam-node-find)
         :map org-mode-map
         (("C-c n l" . org-roam-buffer-toggle)
          ("C-c n i" . org-roam-node-insert)
          ("C-c n r" . org-roam-ref-find)
          ("C-c n g" . org-roam-show-graph))))

;; Update org-id-locations, which are now used by org-roam (as of v2).
;;
;; - https://org-roam.discourse.group/t/org-roam-v2-org-id-id-link-resolution-problem/1491/4
;; - https://lists.gnu.org/archive/html/emacs-orgmode/2009-11/msg01195.html
(defun my/org-id-update-org-roam-files ()
  "Update Org-ID locations for all Org-roam files."
  (interactive)
  (org-id-update-id-locations (org-roam--list-all-files)))

(defun my/org-id-update-id-current-file ()
  "Scan the current buffer for Org-ID locations and update them."
  (interactive)
  (org-id-update-id-locations (list (buffer-file-name (current-buffer)))))
