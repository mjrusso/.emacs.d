;;; ghostel-module-tests.el --- Conditional native installation checks -*- lexical-binding: t; -*-

(require 'ert)
(load (expand-file-name "scripts/sync-ghostel-module.el" user-emacs-directory) nil t)

(ert-deftest my/ghostel-module-sync-installs-only-when-needed ()
  (let* ((directory (make-temp-file "ghostel-module-test-" t))
         (module (expand-file-name (concat "ghostel-module" module-file-suffix) directory))
         (sidecar (expand-file-name "ghostel-module.version" directory))
         (wrapper (expand-file-name "voom-egress-skip" directory))
         (exec-path (cons directory exec-path)))
    (unwind-protect
        (progn
          (with-temp-file wrapper
            (insert "#!/bin/sh\nprintf '%s\\n' 'fixture install failed' >&2\nexit 1\n"))
          (set-file-modes wrapper #o755)
          (with-temp-file sidecar (insert ghostel--minimum-module-version))
          (let ((problem (should-error (my/sync-ghostel-module directory))))
            (should (string-match-p "fixture install failed" (error-message-string problem))))
          (with-temp-file module (insert "fixture"))
          (dolist (version (list ghostel--minimum-module-version
                                 (concat ghostel--minimum-module-version ".1")))
            (with-temp-file sidecar (insert version "\n"))
            (my/sync-ghostel-module directory))
          (dolist (version '(nil "unknown" "0.0.0"))
            (if version
                (with-temp-file sidecar (insert version "\n"))
              (delete-file sidecar))
            (let ((problem (should-error (my/sync-ghostel-module directory))))
              (should (string-match-p "fixture install failed" (error-message-string problem))))))
      (delete-directory directory t))))

(ert-deftest my/ghostel-module-sync-retries-invalid-downloads ()
  (let ((directory (make-temp-file "ghostel-module-test-" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'ghostel--download-module)
                   (lambda (destination &rest _)
                     (with-temp-file
                         (expand-file-name (concat "ghostel-module" module-file-suffix)
                                           destination)
                       (insert "invalid module"))
                     (ghostel--write-module-sidecar-version
                      destination ghostel--minimum-module-version)
                     t)))
          (should-error (my/install-ghostel-module directory))
          (should-not (file-exists-p (ghostel--module-sidecar-path directory))))
      (delete-directory directory t))))
