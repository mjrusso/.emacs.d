(use-package lsp-mode
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)

(use-package lsp-ivy
  :after (lsp-mode ivy)
  :commands lsp-ivy-workspace-symbol)

;; https://github.com/magit/magit/issues/3415#issuecomment-378941991
(use-package git-commit)

(use-package highlight-indentation)
(use-package idle-highlight-mode)

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

(use-package magit
  :defer t
  :config
  ;; Display most magit buffers in the currently-selected window (unless the
  ;; buffer's mode derives from ~magit-diff-mode~ or ~magit-process-mode~, in
  ;; which case it will get a new window).
  ;;
  ;; - https://github.com/magit/magit/issues/2541
  ;; - https://github.com/magit/magit/pull/2656
  (setq magit-display-buffer-function
        'magit-display-buffer-same-window-except-diff-v1)
  ;; Display 20 commits, for example, in the recent commits section.
  (setq magit-log-section-commit-count 20))


;; Highlight matching parentheses when the point is on them.
;; - https://www.emacswiki.org/emacs/ShowParenMode
(setq show-paren-delay 0)
(show-paren-mode 1)

(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode))

(add-hook 'python-mode-hook (lambda () (highlight-indentation-mode)))

;; To use LSP with Rust, ensure RLS is installed: https://github.com/rust-lang/rls
;; Run: `rustup component add rls rust-analysis rust-src`
(use-package rust-mode
  :defer t
  :init
  (setq rust-format-on-save t)
  :hook (rust-mode . lsp)
  (rust-mode . (lambda () (setq indent-tabs-mode nil))))

;; For more on using LSP with Swift, see https://github.com/emacs-lsp/lsp-sourcekit
(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :defer t
  :hook (swift-mode . lsp))

(use-package js2-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode)))

(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.css\\'")
  :config
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t))

;; Prefer node_modules/.bin/prettier, when available.
;;
;; - https://github.com/prettier/prettier-emacs#using-node_modulesbinprettier
;; - https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path :defer t)
(use-package prettier-js
  :defer t
  :init
  (add-hook 'js2-mode-hook 'add-node-modules-path)
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(use-package clojure-mode
  :mode ("\\.clj\\'"
         "\\.cljs\\'"
         "\\.edn\\'"))
