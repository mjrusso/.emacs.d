(use-package ivy
  :init
  (setq ivy-height 15)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-re-builders-alist
        ;; Completion style choices are:
        ;;   ivy--regex
        ;;   ivy--regex-plus
        ;;   ivy--regex-ignore-order
        ;;   ivy--regex-fuzzy
        ;;   regexp-quote
        '((t . ivy--regex-plus))
        )
  (setq ivy-display-style 'fancy)
  (setq swiper-include-line-number-in-search t)
  :config
  (ivy-mode)
  ;; When ivy displays a list of buffers, by default it will run a number of
  ;; transformations on each buffer before displaying them. This replaces the
  ;; transformer function to simply display the buffer name without
  ;; transformation. The motivation for this is to improve performance when
  ;; listing buffers.
  (ivy-configure 'internal-complete-buffer
    :display-transformer-fn '(lambda (str) str))
  :bind (("C-x b" . ivy-switch-buffer)))

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package swiper
  :after ivy
  ;; Once the Swiper minibuffer is active:
  ;;
  ;; - M-n (ivy-next-history-element) will get symbol-at-point into the
  ;;   minibuffer
  ;; - M-j (ivy-yank-word) will extend the minibuffer input with the next
  ;;   word (like using C-s C-w with isearch)
  ;;
  ;; See: https://github.com/abo-abo/swiper/issues/260#issuecomment-147411904n
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch)
         :map ivy-minibuffer-map
         (
          ;; When the selected candidate is a directory, continue completion
          ;; with that directory, rather than open dired.
          ("RET" . ivy-alt-done)
          ;; Support C-s/C-r in the ivy minibuffer.
          ("C-s" . ivy-next-line)
          ("C-r" . ivy-previous-line)
          )))

;; https://github.com/tumashu/ivy-posframe
(use-package ivy-posframe
  :config
  (progn
    (setq ivy-posframe-border-width 1
          ivy-posframe-hide-minibuffer t
          ivy-posframe-min-width 60
          ivy-posframe-parameters '((alpha 100 100)
                                    (max-width . 180)
                                    (left-fringe . 8)
                                    (right-fringe . 4))
          ivy-posframe-display-functions-alist '((swiper          . ivy-display-function-fallback)
                                                 (swiper-isearch  . ivy-display-function-fallback)
                                                 (complete-symbol . ivy-posframe-display-at-point)
                                                 (counsel-M-x     . ivy-posframe-display-at-frame-center)
                                                 (t . ivy-posframe-display-at-window-center)))
    (set-face-attribute 'ivy-posframe nil :foreground "white" :background "#1E3D58") ; "#120022" "#1D253B"
    (ivy-posframe-mode 1)))
