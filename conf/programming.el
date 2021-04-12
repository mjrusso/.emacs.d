(require 'lsp-mode)

;; https://github.com/magit/magit/issues/3415#issuecomment-378941991
(require 'git-commit)

(require 'highlight-indentation)
(require 'idle-highlight-mode)

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(setq-default fill-column 79)

(setq whitespace-line-column 80)
(setq whitespace-style '(face trailing lines-tail tabs))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; If there's an error, ensure that it's visible; otherwise, keep scrolling the
;; compilation buffer.
(setq compilation-scroll-output 'first-error)

;; Don't stop scrolling when encountering warnings.
(setq compilation-skip-threshold 2)

;; Properly interpret control sequences (colours!) in compilation buffers.
;;
;; - https://github.com/atomontage/xterm-color#compilation-buffers
(setq compilation-environment '("TERM=xterm-256color"))
(defun mjr/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'mjr/advice-compilation-filter)

(require 'magit)

;; Display most magit buffers in the currently-selected window (unless the
;; buffer's mode derives from ~magit-diff-mode~ or ~magit-process-mode~, in
;; which case it will get a new window).
;;
;; - https://github.com/magit/magit/issues/2541
;; - https://github.com/magit/magit/pull/2656
(setq magit-display-buffer-function
      'magit-display-buffer-same-window-except-diff-v1)

;; Display 20 commits, for example, in the recent commits section.
(setq magit-log-section-commit-count 20)

;; Highlight matching parentheses when the point is on them.
;; - https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)

(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(add-hook 'python-mode-hook (lambda () (highlight-indentation-mode)))

(require 'rust-mode)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; To use LSP with Rust, ensure RLS is installed: https://github.com/rust-lang/rls
;; Run: `rustup component add rls rust-analysis rust-src`
(add-hook 'rust-mode-hook #'lsp)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-hook 'web-mode-hook  'my-web-mode-hook)
(defun my-web-mode-hook ()
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(require 'prettier-js)
(add-hook 'web-mode-hook 'prettier-js-mode)

(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
