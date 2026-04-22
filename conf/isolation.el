;; https://protesilaos.com/emacs/beframe
(use-package beframe
  :demand t

  :config

  ;; The original `beframe-read-buffer' ignores the predicate. However,
  ;; `tags.el' uses the predicate, so we have to correct this function to use
  ;; the predicate.
  (defun beframe-read-buffer (prompt &optional def require-match predicate)
    "The `read-buffer-function' that limits buffers to frames.
PROMPT, DEF, REQUIRE-MATCH, and PREDICATE are the same as `read-buffer'."
    (completing-read
     (format "%s%s" (beframe--propertize-prompt-prefix) prompt)
     (beframe-completion-table)
     #'(lambda (name)
         (and (beframe--read-buffer-p name)
              (if predicate (funcall predicate name) t)))
     require-match
     nil
     'beframe-history
     def))

  (defun my/beframe-assume-buffer (buffer)
    "Assume BUFFER into the current frame's list using beframe.
BUFFER must be a valid buffer object.
Signals an error if BUFFER is not a buffer object."
    (unless (bufferp buffer)
      (error "Argument BUFFER must be a buffer object, but got %S" buffer))
    (beframe--modify-buffer-list :assume (list buffer) t))

  ;; Wire up the list of beframe-filtered buffers as a candidate source. (These
  ;; will show up first when calling `consult-buffer'.) See
  ;; https://github.com/minad/consult#multiple-sources for more details on how
  ;; this works.
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defun my/beframe-buffer-names-sorted (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
      (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'my/beframe-buffer-names-sorted
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source))

  (beframe-mode))
