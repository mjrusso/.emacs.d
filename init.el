;; Turn off the mouse interface early in startup to avoid momentary display.
(progn
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(magit))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(dolist (l (directory-files (concat user-emacs-directory "lib") nil "^[^\.]"))
  (message "Adding load-path %s" l)
  (add-to-list 'load-path (concat user-emacs-directory "lib/" l))
  (autoload (intern l) (concat l ".el")))

(mapc 'load (directory-files (concat user-emacs-directory "conf")
                             t "^[^#].*el$"))

(when (not (file-exists-p (concat user-emacs-directory "my-autoload.el")))
  (pnh-reinit-libs))

(load (concat user-emacs-directory "my-autoload.el"))

;; Additional context:
;; - https://www.reddit.com/r/emacs/comments/53zpv9/how_do_i_get_emacs_to_stop_adding_custom_fields/d8llk8u/
;; - https://irreal.org/blog/?p=5630
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
   (when (file-exists-p custom-file)
     (load custom-file))

(setq custom-theme-load-path
      (directory-files (concat user-emacs-directory "themes") t "^[^\.]"))

(load-theme 'darkokai t)
;(load-theme 'monokai t)
;(load-theme 'zenburn t)
;(load-theme 'twilight t)
;(load-theme 'solarized t)
;(load-theme 'espresso t)
;(load-theme 'tango-plus t)

(message "Shell: %s\nPath: %s"
         (getenv "SHELL")
         (getenv "PATH"))
