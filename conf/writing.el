(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Use visual line mode by default for markdown files.
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-visual-line-mode)

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
(require 'typo)

(setq-default typo-language "English")

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

;; To use Typo by default for markdown files, uncomment the following line:
;; (add-hook 'markdown-mode-hook 'typo-mode)
