(require 'ivy)
(require 'counsel)
(require 'swiper)

(ivy-mode)
(counsel-mode)

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

;; Support C-s/C-r in the ivy minibuffer.
(define-key ivy-minibuffer-map (kbd "C-s") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-r") 'ivy-previous-line)

;; When the selected candidate is a directory, continue completion with that
;; directory, rather than open dired.
(define-key ivy-minibuffer-map (kbd "RET") 'ivy-alt-done)

;; When ivy displays a list of buffers, by default it will run a number of
;; transformations on each buffer before displaying them. This replaces the
;; transformer function to simply display the buffer name without
;; transformation. The motivation for this is to improve performance when
;; listing buffers.
(ivy-configure 'internal-complete-buffer
  :display-transformer-fn '(lambda (str) str))
