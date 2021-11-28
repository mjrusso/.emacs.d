
;; ;;
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "HOLD(h)" "NOTE(n)" "|" "DONE(d)" "DEAD(x)")))


;; ;; https://github.com/tarsius/hl-todo
;; (setq hl-todo-keyword-faces
;;       '(("TODO" . org-todo)
;;         ("NOTE" . (:foreground "LightSalmon" :weight "bold"))
;;         ("HOLD" . org-done)
;;         ("DEAD" . org-done)))

;; TODO see https://www.reddit.com/r/emacs/comments/mt51h4/strike_through_orgdone_tasks_in_agenda_to/
;;(set-face-attribute 'org-headline-done nil :strike-through t)


;; Use visual line mode by default for org files.
(add-hook 'org-mode-hook 'turn-off-auto-fill)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Use org-indent-mode by default.
(add-hook 'org-mode-hook (lambda () (org-indent-mode t)) t)

(setq org-completion-use-ido t)

(setq org-startup-folded t)

(setq org-startup-with-inline-images t)

;; https://www.reddit.com/r/orgmode/comments/l215r5/terrible_performance_with_inline_images_on_macs/
;; https://www.reddit.com/r/emacs/comments/55zk2d/adjust_the_size_of_pictures_to_be_shown_inside/d8geca2
(setq org-image-actual-width (/ (display-pixel-width) 3))

;; https://github.com/abo-abo/org-download
;; https://www.orgroam.com/manual.html#Org_002ddownload
;; https://zzamboni.org/post/how-to-insert-screenshots-in-org-documents-on-macos/
(use-package org-download
  :after org
  :defer nil
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-screenshot-method "pngpaste %s")
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-clipboard)
         ("s-y" . org-download-yank))))

;; Configure org-agenda.
(setq org-agenda-files (list "~/Dropbox/org-roam/"))

;; Org-roam uses ripgrep (when it's installed) for better performance.
;;
;; ripgrep: https://github.com/BurntSushi/ripgrep

(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (file-truename "~/Dropbox/org-roam/"))
  (org-roam-capture-templates
   '(("d" "default" plain (function org-roam--capture-get-point)
      "%?"
      :file-name "${slug}"
      :head "#+title: ${title}\n"
      :immediate-finish t
      :unnarrowed t)))
  (org-roam-dailies-directory "daily/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      #'org-roam-capture--get-point
      "* %?"
      :file-name "daily/%<%Y-%m-%d>"
      :head "#+title: %<%Y-%m-%d>\n\n")))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph)
               ("C-c n t" . org-roam-dailies-capture-today))
         :map org-mode-map
              (("C-c n i" . org-roam-insert)
               ("C-c n I" . org-roam-insert-immediate))))

;; Configure Deft. (http://jblevins.org/projects/deft/)
;; Additional resources:
;; - https://www.orgroam.com/manual.html#Full_002dtext-search-interface-with-Deft
;; - http://emacs-fu.blogspot.ca/2011/09/quick-note-taking-with-deft-and-org.html
;; - http://www.jontourage.com/2013/08/15/setting-up-deft-mode-in-emacs-with-org-mode/
;; - http://www.emacswiki.org/emacs/DeftMode

(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory)
  (deft-auto-save-interval 0))
