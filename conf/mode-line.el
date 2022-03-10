;; A minor-mode menu for the mode line, which hides the list of enabled
;; minor-modes that are displayed in the mode line by default.
;;
;; - https://github.com/tarsius/minions
(use-package minions
  :init
  (defface my/minions-mode-line-face
    '((t
       :weight normal
       :foreground "MediumPurple1"
       :background nil
       :box nil))
    "Face for the minions mode line."
    :group 'my/faces)
  (setq minions-mode-line-face 'my/minions-mode-line-face
        minions-mode-line-lighter "ϟ"
        minions-mode-line-delimiters '("" . ""))
  :config
  (minions-mode 1))

;; Mode line tweaks. With thanks to:
;; https://github.com/drusso/.emacs.d/blob/main/conf/mode-line.el

;; Override the `mode-line' face.
(set-face-attribute 'mode-line nil
                    :foreground "#656868"
                    :background "#23334a"
                    :box '(:line-width 5 :color "#23334a"))

;; Override the `mode-line-inactive' face. It has a darker colour than the
;; active face.
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#656868"
                    :background "#1c212e"
                    :box '(:line-width 5 :color "#1c212e"))

(defface my/mode-line-file-name-face
  '((t
     :weight bold
     :foreground "SpringGreen2"
     :background nil
     :box nil))
  "Face for the file's name (or the buffer name if the buffer
isn't visiting a file)."
  :group 'my/faces)

(defface my/mode-line-file-name-modified-face
  '((t
     :weight bold
     :foreground "firebrick1"
     :background nil
     :box nil))
  "Face for the file's name if the file is modified."
  :group 'my/faces)

(defface my/mode-line-project-face
  '((t
     :weight normal
     :foreground "#8db6cd" ;; This colour is "SkyBlue3".
     :background nil
     :box nil))
  "Face for the file's project."
  :group 'my/faces)

(defface my/mode-line-file-dir-face
  '((t
     :weight normal
     :foreground "LemonChiffon2"
     :background nil
     :box nil))
  "Face for the file's directory."
  :group 'my/faces)

(defface my/mode-line-vc-branch-face
  '((t
     :weight normal
     :slant italic
     :foreground "#9f79ee" ;; This colour is "MediumPurple2".
     :background nil
     :box nil))
  "Face for the version control branch."
  :group 'my/faces)

(defface my/mode-line-cursor-position-face
  '((t
     :weight normal
     :foreground "#8b7b8b" ;; This colour is "thistle4".
     :background nil
     :box nil))
  "Face for the line/column info."
  :group 'my/faces)

;; An `alist' cache of values that are (relatively) expensive to compute on
;; every mode line redraw.
(setq my/mode-line-cache nil)

(defun my/mode-line-populate-cache ()
  "Populate all of the values (buffer local) we keep in the cache
for each mode line redraw."
  (setq-local my/mode-line-cache
              `((perspective-name . ,(persp-current-name))
                (project-name . ,(projectile-project-name))
                (project-root . ,(projectile-project-root))
                (buffer-file-name . ,(buffer-file-name))
                (file-name-nondirectory . ,(let ((name (buffer-file-name)))
                                             (if name (file-name-nondirectory name) nil)))
                (file-path-project-relative . ,(let ((name (buffer-file-name))
                                                     (project (projectile-project-name)))
                                                 (if name
                                                     (let ((dir (file-name-directory name)))
                                                       (if project
                                                           (file-relative-name dir (projectile-project-root))
                                                         dir))
                                                   ""))))))

(defun my/mode-line-get (key)
  "Get a value from the mode line values cache. If the cache
isn't yet populated, populate it first."
  (if my/mode-line-cache
      nil
      (my/mode-line-populate-cache))
  (cdr (assoc key my/mode-line-cache)))

(defun my/mode-line-file-name-or-buffer-name ()
  "For use in `mode-line-format'. Returns the file name if the
current buffer is visiting a file, otherwise returns the '%b'
construct (which will print the buffer name as-is)."
  (let ((name (my/mode-line-get 'file-name-nondirectory)))
    (or name "%b")))

(defun my/mode-line-project ()
  "For use in `mode-line-format'. Accesses the perspective name
and the project names (from cache). The string return value
differentiates between various cases: perspective name and
project name are the same; there is no associated project; and,
the perspective and project names are different."
  (let ((perspective (my/mode-line-get 'perspective-name))
        (project (my/mode-line-get 'project-name)))
    (cond ((string= perspective project) (format "{%s} " perspective))
          ((string= project "-") (format "(%s) " perspective))
          ((format "[%s|%s] " perspective project)))))

(defun my/mode-line-file-dir ()
  "For use in `mode-line-format'. If the buffer is visiting a
file and the file is part of a projectile project, returns the
directory path relative to the project root (best used in
conjunction with `my/mode-line-project'). If the buffer is
visiting a file and the the file is not part of a projectile
project, the directory path is returned. Returns an empty string
if the the file is not visiting a file."
  (my/mode-line-get 'file-path-project-relative))

(setq-default my/mode-line-format-left
              (list
               mode-line-front-space

               ;; The default emacs mode line includes the following, but they
               ;; are omitted here:
               ;;
               ;;   `mode-line-mule-info'
               ;;   `mode-line-client'
               ;;   `mode-line-remote'
               ;;
               ;; We also omit the %p and %I constructs, which are the position
               ;; in the buffer (the percent above the top window), and the
               ;; overall size of the buffer, respectively.

               mode-line-modified

               " "

               ;; The file (or buffer) name. The face is selected based on
               ;; whether the buffer is modified or not.
               '(:eval
                 (let ((name (my/mode-line-file-name-or-buffer-name))
                       (face
                        (if (buffer-modified-p)
                            'my/mode-line-file-name-modified-face
                          'my/mode-line-file-name-face))
                       ;; (face 'my/mode-line-file-name-face)
                       )
                   (propertize name
                               'face face
                               'help-echo (buffer-name))))

               " "

               ;; The buffer's project and/or file's directory (which may be
               ;; relative to the project root if we're in a project).
               '(:eval
                 (propertize (my/mode-line-project)
                             'face 'my/mode-line-project-face))
               '(:eval
                 (propertize (my/mode-line-file-dir)
                             'face 'my/mode-line-file-dir-face))

               " "

               ;; The line and column counts (%c starts the column count at 0,
               ;; and %C at 1).
               (propertize "(%l,%c)" 'face 'my/mode-line-cursor-position-face)

               " "

               ;; The version control branch.
               '(:eval
                 (propertize (if vc-mode (substring vc-mode 5) "")
                             'face 'my/mode-line-vc-branch-face
                             'help-echo nil))
               ))

(setq-default my/mode-line-format-right
              (list
               minions-mode-line-modes ; Instead of `mode-line-modes`.
               " "
               mode-line-misc-info
               ))

(defun my/render-mode-line (left-format right-format)
  (let* ((left (format-mode-line left-format))
         (right (format-mode-line right-format))
         (space (propertize
                 " "
                 'display `((space
                             :align-to
                             (- (+ right right-fringe right-margin)
                                ,(+ (+ 3 (string-width right)))))))))
    (format " %s%s%s " left space right)))

;; Set the (default) template for the mode line.
(setq-default mode-line-format
              '((:eval (my/render-mode-line
                        my/mode-line-format-left
                        my/mode-line-format-right))))
