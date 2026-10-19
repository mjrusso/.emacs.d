;;; sync-ghostel-module.el --- Install required Ghostel native modules -*- lexical-binding: t; -*-

(require 'ghostel-module-install)

(defun my/ghostel-module-current-p (directory)
  "Return non-nil if DIRECTORY has a module at least as new as Ghostel requires."
  (and (file-exists-p (expand-file-name (concat "ghostel-module" module-file-suffix)
                                      directory))
       (when-let* ((version (ghostel--read-module-sidecar-version directory)))
         (condition-case nil
             (version<= ghostel--minimum-module-version version)
           (error nil)))))

(defun my/install-ghostel-module (directory)
  "Download and validate Ghostel's required native module in DIRECTORY."
  (unless (ghostel--download-module directory ghostel--minimum-module-version)
    (error "Ghostel native module download failed"))
  (condition-case problem
      (progn
        (module-load (expand-file-name (concat "ghostel-module" module-file-suffix)
                                     directory))
        (unless (version<= ghostel--minimum-module-version (ghostel--module-version))
          (error "Installed Ghostel module does not meet the required version")))
    (error
     (delete-file (ghostel--module-sidecar-path directory))
     (signal (car problem) (cdr problem)))))

(defun my/sync-ghostel-module (directory)
  "Install a missing or outdated module in DIRECTORY."
  (if (my/ghostel-module-current-p directory)
      (message "Ghostel native module is current")
    (let* ((emacs (expand-file-name invocation-name invocation-directory))
           (wrapper (executable-find "voom-egress-skip"))
           (arguments
            (append (when wrapper (list "--" emacs))
                    (list "-Q" "--batch"
                          "-L" (file-name-directory (locate-library "compat"))
                          "-L" (file-name-directory (locate-library "ghostel-module-install"))
                          "-l" (expand-file-name "scripts/sync-ghostel-module.el"
                                                 user-emacs-directory)
                          "--eval" (prin1-to-string
                                    `(my/install-ghostel-module ,(expand-file-name directory)))))))
      (message "Installing Ghostel native module %s%s"
               ghostel--minimum-module-version
               (if wrapper " via voom-egress-skip" ""))
      (with-temp-buffer
        (unless (zerop (apply #'process-file (or wrapper emacs) nil '(t t) nil arguments))
          (error "Ghostel native module installation failed: %s"
                 (string-trim (buffer-string))))
        (message "%s" (string-trim (buffer-string)))))))
