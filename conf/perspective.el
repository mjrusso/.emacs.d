;; Support for multiple named workspaces ("perspectives").
;;
;; https://github.com/nex3/perspective-el

(use-package perspective

  :bind
  (("C-x k" . persp-kill-buffer*)
   ("M-}" . (lambda () (interactive) (my/call-func-if-minibuffer-not-open 'persp-next)))
   ("M-{" . (lambda () (interactive) (my/call-func-if-minibuffer-not-open 'persp-prev)))
   ("C-c =" . persp-switch)
   ("C-c _" . 'my/open-project)
   ("C-c +" . 'my/open-project-in-new-perspective)
   ("C-c 0" . persp-kill))

  :config
  ;; Bind C-c 1 through C-c 9 to specific perspectives.
  (dotimes (i 9)
    (let ((d (+ i 1)))
      (global-set-key (kbd (format "C-c %d" d))
                      `(lambda ()
                         (interactive)
                         (persp-switch-by-number ,d)))))

 :init
 (setq persp-initial-frame-name "scratch")
 (setq persp-show-modestring nil)
 (setq persp-modestring-short nil)
 (setq persp-sort 'created)
 (setq persp-suppress-no-prefix-key-warning t)
 ;; Set the text corresponding to the perspective mode line as the frame title.
 (setq frame-title-format
       (list '(:eval (my/perspective-names))))
 (persp-mode))

(defun my/call-func-if-minibuffer-not-open (persp-change-func)
  (if (active-minibuffer-window)
         (error "Preventing perspective switch while minibuffer open")
       (funcall persp-change-func)))

(defun my/perspective-names ()
  "Returns the names of all perspectives. The currently-active
perspective name is specially marked, to make it clearer which
perspective is currently active."
  (persp-intersperse
   (mapcar
    #'(lambda (name)
        (if (eq name (persp-current-name))
            (concat "*" name "*")
          name))
    (persp-names))
   '"   —   "))

;; Group buffers by perspective name in the ibuffer buffer.
(add-hook 'ibuffer-hook
          (lambda ()
            (persp-ibuffer-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; As an alternative to using `persp-switch-to-buffer*', add the
;; perspective-filtered buffer list as a virtual buffer source to consult. This
;; makes the list of buffers in the current perspective available to
;; `consult-buffer'.
;;
;; For more details on how this works, see
;; <https://github.com/minad/consult#multiple-sources>.

(defvar my/consult--source-perspective-buffer
  `(:name     "Perspective Buffer"
    :hidden   nil
    :category buffer
    :face     consult-buffer
    :history  buffer-name-history
    :default  t
    :state    ,#'consult--buffer-state
    :enabled  ,(lambda () t)
    :items    ,(lambda ()
                 (consult--buffer-query :sort 'visibility
                                        :predicate (lambda (arg) (not (persp-buffer-filter arg)))
                                        :as #'buffer-name)))
  "Perspective buffer candidate source for `consult-buffer'.")

(defun my/add-perspective-virtual-buffer-source-to-consult ()
  "Add all perspective-filtered buffers as a `consult-buffer'
virtual buffer source."
  (add-to-list 'consult-buffer-sources 'my/consult--source-perspective-buffer))
