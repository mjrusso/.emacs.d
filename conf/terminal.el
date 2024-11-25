;; WIP support for improving Emacs in the terminal.
;;
;;(With thanks to @drusso for sharing his config.)

;; (use-package emacs
;;   :config
;;   (setq
;;    select-enable-clipboard nil
;;    select-enable-primary nil))

;; (use-package emacs
;;   :bind (("M-W" . clipboard-kill-ring-save)))

(use-package xclip :ensure t)
(use-package clipetty :ensure t)

(if (not (display-graphic-p))
    (if (or (executable-find "pbcopy")
            (executable-find "xclip"))
        ;; xclip uses external programs such as xclip and pbcopy.
        ;;
        (use-package xclip
          :init
          (xclip-mode 1))

      ;; https://github.com/spudlyo/clipetty
      ;;
      ;; `clipetty' uses OSC 52 escape sequences to send text killed in Emacs
      ;; to the system clipboard.
      (use-package clipetty
        :ensure t
        :hook (after-init . global-clipetty-mode)
        )))

(cl-defstruct my/modifier-chord id esc0 esc1 mods)

(defconst my/modifier-chords
  `(,(make-my/modifier-chord :id 'C :mods '(control)
                             :esc0 "\e\[27;5;%d~" :esc1 "\e\[%d;5u")
    ,(make-my/modifier-chord :id 'M :mods '(meta)
                             :esc0 "\e\[27;3;%d~" :esc1 "\e\[%d;3u")
    ,(make-my/modifier-chord :id 'C-M :mods '(control meta)
                             :esc0 "\e\[27;7;%d~" :esc1 "\e\[%d;7u")
    ,(make-my/modifier-chord :id 'C-S :mods '(control shift)
                             :esc0 "\e\[27;6;%d~" :esc1 "\e\[%d;6u")
    ,(make-my/modifier-chord :id 'C-M-S :mods '(control meta shift)
                             :esc0 "\e\[27;8;%d~" :esc1 "\e\[%d;8u")
    ,(make-my/modifier-chord :id 'M-S :mods '(meta shift)
                             :esc0 "\e\[27;4;%d~" :esc1 "\e\[%d;4u")))

(defconst my/modifier-chords-by-id
  (mapcar #'(lambda (c) `(,(my/modifier-chord-id c) . ,c)) my/modifier-chords))

(defconst my/char-modifiers-table
  `(,@(mapcar
       #'(lambda (c) `(,c C M C-M C-S C-M-S M-S))
       `(13 127 ,@(number-sequence 32 126)))))

(defun my/register-char-with-modifier-chord (char modifier-chord-id)
  (let* ((modifier-chord
          (alist-get modifier-chord-id my/modifier-chords-by-id))
         (mods (my/modifier-chord-mods modifier-chord))
         (esc0 (my/modifier-chord-esc0 modifier-chord))
         (esc1 (my/modifier-chord-esc1 modifier-chord))
         (key0 (if esc0 (format esc0 char) nil))
         (key1 (if esc1 (format esc1 char) nil))
         (event-vec (my/char-modifiers-event-vector char mods)))
    (if key0 (define-key xterm-function-map key0 event-vec))
    (if key1 (define-key xterm-function-map key1 event-vec))))

;; This function is sourced from:
;;
;;   https://gist.github.com/gnachman/b4fb1e643e7e82a546bc9f86f30360e4"
;;
(defun my/char-modifiers-event-vector (c modifiers)
  "Apply modifiers to the character C. MODIFIERS must be a list of
symbols amongst (meta control shift). Return an event vector."
  (if (memq 'control modifiers) (setq c (if (and (<= ?a c) (<= c ?z))
                                            (logand c ?\x1f)
                                          (logior (lsh 1 26) c))))
  (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
  (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
  (vector c))

;; This is adapted from:
;;
;;   https://gist.github.com/gnachman/b4fb1e643e7e82a546bc9f86f30360e4
;;
(defun my/handle-more-terminal-escapes ()
  "Handle additional terminal escape sequences (CSI u), as
documented at [1].

This is required for Emacs with terminal-based input to handle
some keys with modifiers in the same manner as windowed Emacs
(ex. on macOS), such as 'C-1', 'C-2', 'C-,', 'C-;', 'C-.', and
'C-m'.

Terminals typically need to be configured to send these alternate
sequences. (In iTerm, enable 'Report modifiers using CSI u'.)
Otherwise, they use more conventional behaviour. For example, a
key press of 'C-2'is transmitted by the terminal as 'C-@',which
is bound to `set-mark-command', not the prefix 'C-u 2'.

[1]: http://www.leonerd.org.uk/hacks/fixterms/"
  (when (and (boundp 'xterm-extra-capabilities)
             (boundp 'xterm-function-map))
    (dolist (entry my/char-modifiers-table)
      (let ((char (car entry))
            (mods (cdr entry)))
        (dolist (mod mods)
          (my/register-char-with-modifier-chord char mod))))))

(use-package emacs
  :config
  (setq xterm-extra-capabilities '(modifyOtherKeys))
  (setq xterm-set-window-title t)
  (when (not (window-system))
    (if (not (eq (tty-display-color-cells) 16777216))
        ;; https://chadaustin.me/2024/01/truecolor-terminal-emacs/
        (warn "TTY does not appear to support 24 bit colour")
      ))
  :hook
  (tty-setup . my/handle-more-terminal-escapes)
  )

;; (set 'xterm-standard-colors
;;   '(("black"          0 (  0   0   0))
;;     ("red"            1 (255   0   0))
;;     ("green"          2 (  0 255   0))
;;     ("yellow"         3 (255 255   0))
;;     ("blue"           4 (  0   0 255))
;;     ("magenta"        5 (255   0 255))
;;     ("cyan"           6 (  0 255 255))
;;     ("white"          7 (255 255 255))
;;     ("brightblack"    8 (127 127 127))
;;     ("brightred"      9 (255   0   0))
;;     ("brightgreen"   10 (  0 255   0))
;;     ("brightyellow"  11 (255 255   0))
;;     ("brightblue"    12 (92   92 255))
;;     ("brightmagenta" 13 (255   0 255))
;;     ("brightcyan"    14 (  0 255 255))
;;     ("brightwhite"   15 (255 255 255)))
;;   )
