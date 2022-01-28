;; Disable the tab bar (not yet supported on macOS --with-ns builds). This does
;; not disable tabs, only the visual bar.
(setq tab-bar-show nil)

;; On initialization, name the initial tab.
(tab-rename "untitled")

(defun my/new-tab ()
  "Create a new tab, prompting for its name first."
  (interactive)
  (let* ((name (read-from-minibuffer "New tab: "))
         (tab (tab-new)))
    (if name (tab-rename name))))

(defun my/current-tab-index ()
  "Returns the tab index prefixed with # and 1-indexed."
  (format "#%d"
          (+ (tab-bar--current-tab-index) 1)))

(defun my/current-tab-name-with-index ()
  "Returns the tab index (prefixed with # and 1-indexed) and the
name of the tab."
  (format "%s - %s"
          (my/current-tab-index)
          (alist-get 'name (tab-bar--current-tab))))
