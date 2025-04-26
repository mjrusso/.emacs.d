;; https://protesilaos.com/emacs/beframe
(use-package beframe
  :init
  (beframe-mode)

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

  )
