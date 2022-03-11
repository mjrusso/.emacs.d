(add-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.mdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  ;; Use visual line mode by default for markdown files.
  (add-hook 'markdown-mode-hook 'turn-off-auto-fill)
  (add-hook 'markdown-mode-hook 'turn-on-visual-line-mode))

;; Text expansion with abbrev-mode.
;; For more details, see: https://emacs.stackexchange.com/a/20483

(define-abbrev-table 'unicode-table
  '(("ualpha" "α")
    ("ubeta"  "β")
    ("ugamma" "γ")
    ("rarr" "→")
    ("larr" "←")
    ("uarr" "↑")
    ("darr" "↓")
    ("drarr" "⇒")
    ("dlarr" "⇐")
    ("duarr" "⇑")
    ("ddarr" "⇓")
   )
)

(define-abbrev-table 'text-mode-abbrev-table
  nil
  "Text mode abbrev table."
  :parents (list unicode-table))

(add-hook 'text-mode-hook #'abbrev-mode)

;; Typo!

(use-package typo
  :init
  (setq-default typo-language "English"))

(define-typo-cycle typo-cycle-right-single-quotation-mark
  "Cycle through the typewriter apostrophe and the right quotation mark."
  ("'" "’" ))

(define-typo-cycle typo-cycle-left-single-quotation-mark
  "Cycle through the backtick and the left single quotation mark."
  ("`" "‘"))

(define-typo-cycle typo-cycle-dashes
  "Cycle through various dashes."
  ("-" ; HYPHEN-MINUS
   "—" ; EM DASH
   "–" ; EN DASH
   "−" ; MINUS SIGN
   "‐" ; HYPHEN
   "‑" ; NON-BREAKING HYPHEN
  ))

;; typo-global-mode provides a globally-accessible key map (using 'C-c 8', to
;; complement Emacs's default 'C-x 8' prefix map), for inserting various
;; Unicode characters. For example, use 'C-c 8 - >' to insert a '→' character.
(typo-global-mode 1)

;; Use Typo with text-mode...
(add-hook 'text-mode-hook 'typo-mode)

;; ...but disable it when we're in an org-mode src block.
;; https://emacs.stackexchange.com/a/56167
(defun disable-typo-in-org-src-block ()
  (add-hook 'typo-disable-electricity-functions 'org-in-src-block-p nil :local))

(add-hook 'org-mode-hook 'disable-typo-in-org-src-block)

;; ...and also when we're editing inline code in a Markdown document.
(defun disable-typo-in-markdown-code-block ()
  (add-hook 'typo-disable-electricity-functions 'markdown-code-block-at-point-p nil :local))

(defun disable-typo-in-markdown-inline-code ()
  (add-hook 'typo-disable-electricity-functions 'markdown-inline-code-at-point-p nil :local))

(add-hook 'markdown-mode-hook 'disable-typo-in-markdown-code-block)

(add-hook 'markdown-mode-hook 'disable-typo-in-markdown-inline-code)
