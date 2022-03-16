(use-package lsp-mode
  :defer t
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq lsp-headerline-breadcrumb-enable nil)
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

(use-package idle-highlight-mode
  :config
  (setq idle-highlight-exclude-point t)
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

;; Highlight uncommitted changes in the gutter, using a mix of diff-hl and
;; git-gutter/git-gutter-fringe, depending on the context.
;;
;; - https://github.com/dgutov/diff-hl
;; - https://github.com/emacsorphanage/git-gutter
;; - https://github.com/emacsorphanage/git-gutter-fringe
;;
;; Also see:
;;
;; - https://ianyepan.github.io/posts/emacs-git-gutter/
(use-package diff-hl
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
;;  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
  )

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;; Highlight indentation levels.
;; - https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-responsive 'stack)
  (setq highlight-indent-guides-auto-character-face-perc 8)
  (setq highlight-indent-guides-character ?\┆)
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

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
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

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

(use-package smartparens
  :hook ((prog-mode . smartparens-mode)
         (ielm-mode-hook . smartparens-mode))
  :init
  (require 'smartparens-config))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package cargo
  :pretty-hydra
  ((:quit-key "q")
   ("Packages"
    (("a" cargo-process-add "add")
     ("u" cargo-process-update "update")
     ("x" cargo-process-rm "remove")
     ("U" cargo-process-upgrade "upgrade"))
    "Actions"
    (("b" cargo-process-build "build")
     ("r" cargo-process-run "run")
     ("c" cargo-process-check "check")
     ("f" cargo-process-format "format")
     ("s" cargo-process-search "search"))
    "Tests"
    (("t" cargo-process-current-file-tests "test")
     ("T" cargo-process-test "test all")))))

;; To use LSP with Rust, ensure RLS is installed: https://github.com/rust-lang/rls
;; Run: `rustup component add rls rust-analysis rust-src`
(use-package rust-mode
  :defer t
  :init
  (setq rust-format-on-save t)
  :hook (rust-mode . lsp)
  (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  :mode-hydra
  ((:color teal :quit-key "q")
   ("Quick Action"
    (("b" cargo-process-build "build")
     ("t" cargo-process-current-file-tests "test")
     ("T" cargo-process-test "test all")
     ("R" cargo-process-run "run")
     ("C" cargo-hydra/body "cargo...")))))

;; For more on using LSP with Swift, see https://github.com/emacs-lsp/lsp-sourcekit
(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :defer t
  :hook (swift-mode . lsp))

;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path :defer t)

;; https://github.com/prettier/prettier-emacs
(use-package prettier-js :defer t)

;; For more on using LSP with Typescript, see:
;; https://emacs-lsp.github.io/lsp-mode/page/lsp-typescript/
;;
;; Note that the typescript-language-server, *and* typescript (`tsc`), must be
;; available in the current path. Instead of a global install (i.e., `npm i -g
;; typescript typescript-language-server`), rely on the add-node-modules-path
;; package, which adds `node_modules/.bin/` to the current path.
(use-package typescript-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
  :hook
  (typescript-mode . add-node-modules-path)
  (typescript-mode . prettier-js-mode)
  (typescript-mode . lsp-deferred)
  )

(use-package js2-mode
  :defer t
  :init
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
  :hook
  (js2-mode . add-node-modules-path)
  (js2-mode . prettier-js-mode)
  )

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

(use-package clojure-mode
  :mode ("\\.clj\\'"
         "\\.cljs\\'"
         "\\.edn\\'"))

;; Interactively make HTTP requests, for testing RESTful APIs.
;;
;; - https://github.com/pashky/restclient.el
;; - https://erick.navarro.io/blog/testing-an-api-with-emacs-and-restclient/
;; - https://jakemccrary.com/blog/2014/07/04/using-emacs-to-explore-an-http-api/
;;
;; Other options: https://emacs.stackexchange.com/q/2427
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

;; LSP-related Hydras. Adapted from:
;; https://github.com/jerrypnz/.emacs.d/blob/master/lisp/jp-lsp-hydra.el
(major-mode-hydra-define+ (rust-mode swift-mode typescript-mode)
  (:color teal :quit-key "q")
  ("LSP Quick Action"
   (("d" lsp-describe-thing-at-point "describe symbol")
    ("a" lsp-execute-code-action "code action")
    ("f" lsp-format-buffer "format")
    ("O" lsp-organize-imports "organize imports"))
   "Find & Goto"
   (("gr" lsp-ui-peek-find-references "references")
    ("gd" lsp-ui-peek-find-definitions "definitions")
    ("gf" lsp-ivy-workspace-symbol "workspace symbol"))
   "Connection"
   (("cc" lsp "start")
    ("cr" lsp-restart-workspace "restart")
    ("cd" lsp-describe-session "describe session")
    ("cq" lsp-shutdown-workspace "shutdown"))
   "Toggles"
   (("ol" lsp-lens-mode "toggle lens" :toggle t :exit nil)
    ("od" lsp-ui-doc-mode "toggle hover doc" :toggle t :exit nil)
    ("os" lsp-ui-sideline-mode "toggle sideline" :toggle t :exit nil))))
