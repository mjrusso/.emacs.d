;;; magit-tests.el --- Magit menu compatibility checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'magit)

(ert-deftest my/magit-diff-menu-loads ()
  (save-window-excursion
    (with-temp-buffer
      (let ((default-directory user-emacs-directory))
        (unwind-protect
            (progn
              (magit-diff)
              (should (eq (oref transient--prefix command) 'magit-diff))
              (should (get-buffer-window transient--buffer)))
          (transient-quit-all))))))
