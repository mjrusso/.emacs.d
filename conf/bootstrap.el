;; Byte-compile and autoload vendored Emacs packages.
;;
;; For more details, see:
;;
;; - https://github.com/technomancy/dotfiles/blob/master/.emacs.d/phil/bootstrap.el
;; - https://github.com/technomancy/emacs-starter-kit/tree/v3

(defun mjr/reinit-libs ()
  (interactive)
  (let ((generated-autoload-file (concat user-emacs-directory "my-autoload.el")))
    (dolist (d (directory-files (concat user-emacs-directory "lib") t "^[^\.]"))
      (dolist (f (directory-files d t "\\.el$"))
        (byte-compile-file f))
      (update-directory-autoloads d))))
