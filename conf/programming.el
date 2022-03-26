;; https://github.com/joaotavora/eglot
;;
;; Emacs LSP client.
;;
;; Note that Eglot is not currently configured to start automatically; instead,
;; enable manually by running `M-x eglot'.
;;
;; Also note that `C-h .' is mapped to `eglot-help-at-point', but this does not
;; display diagnostic errors at point (only the documentation, if available).
;; It seems like this will be addressed in eldoc (see
;; https://github.com/joaotavora/eglot/issues/454), but for now, to see
;; diagnostic errors, either manually invoke `M-x display-local-help' or use
;; flymake (`M-x consult-flymake' is particularly nice).
(use-package eglot
  :defer t

  :custom

  (eglot-autoshutdown t)

  :config

  ;; Ignore (i.e., disable) document highlight support. (Highlighting the
  ;; symbol under the cursor is distracting.)
  (add-to-list 'eglot-ignored-server-capabilites :documentHighlightProvider)

  :init

  ;; Use Orderless completion with Corfu. See:
  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-eglot
  (setq completion-category-overrides '((eglot (styles orderless))))

  )

;; xref works great with eglot; for example: `xref-find-definitions',
;; `xref-find-references', `xref-go-back', etc.
(use-package xref)

(use-package eldoc
  :custom (eldoc-echo-area-prefer-doc-buffer t))

(use-package flymake
  :hook ((prog-mode . flymake-mode))
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error))
  )

;; https://github.com/mohkale/consult-eglot
;;
;; Currently disabled, because `consult-eglot-symbols' does not seem to work.
;; Not sure if it's an issue with the package, or eglot, or consult, or the
;; specific language servers I've been testing with...
;;
;;      (use-package consult-eglot
;;        :after (eglot consult)
;;        :bind (:map eglot-mode-map ("M-j" . #'consult-eglot-symbols)))


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

(use-package whitespace
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face trailing lines-tail tabs))
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package compile
  :config
  ;; If there's an error, ensure that it's visible; otherwise, keep scrolling
  ;; the compilation buffer.
  (setq compilation-scroll-output 'first-error)

  ;; Don't stop scrolling when encountering warnings.
  (setq compilation-skip-threshold 2)

  ;; Always kill the current compilation process before starting a new one, in
  ;; lieu of prompting.
  (setq compilation-always-kill t)

  ;; Properly interpret control sequences (colours!) in compilation buffers.
  ;;
  ;; - https://github.com/atomontage/xterm-color#compilation-buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my/advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my/advice-compilation-filter)

  ;; (Very hacky) functions to disable (and re-enable) error parsing in
  ;; compilation mode.

  (defun my/disable-error-parsing-in-compilation-mode ()
    (interactive)
    (if (not (boundp 'my/cached-compilation-error-regexp-alist))
        (setq my/cached-compilation-error-regexp-alist compilation-error-regexp-alist))
    (setq compilation-error-regexp-alist nil)
    (message "Error parsing disabled for compilation mode"))

  (defun my/enable-error-parsing-in-compilation-mode ()
    (interactive)
    (if (boundp 'my/cached-compilation-error-regexp-alist)
        (progn
          (setq compilation-error-regexp-alist my/cached-compilation-error-regexp-alist)
          (message "Error parsing enabled for compilation mode"))
      (message "Error parsing already enabled for compilation mode")))

  )

(use-package magit
  :defer t
  :config

  ;; Display magit buffers in the current window, rather than a new window.
  (setq magit-display-buffer-function
        (lambda (buffer)
          (display-buffer buffer '(display-buffer-same-window))))

  ;; Don't show the diff buffer when authoring a commit. To manually bring up the
  ;; diff, use `C-c C-d'.
  ;;
  ;; The `server-switch-hook' change is documented here:
  ;;
  ;;   <https://magit.vc/manual/magit/Performance.html>
  ;;
  (setq magit-commit-show-diff nil)
  (remove-hook 'server-switch-hook 'magit-commit-diff)

  ;; Display 40 commits, for example, in the recent commits section.
  (setq magit-log-section-commit-count 40))

;; Highlight matching parentheses when the point is on them.
;; - https://www.emacswiki.org/emacs/ShowParenMode
(use-package paren
  :init
  (setq show-paren-delay 0)
  (show-paren-mode 1))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook #'rainbow-delimiters-mode))

;; https://github.com/Fuco1/smartparens
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
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  :mode-hydra
  ((:color teal :quit-key "q")
   ("Quick Action"
    (("b" cargo-process-build "build")
     ("t" cargo-process-current-file-tests "test")
     ("T" cargo-process-test "test all")
     ("R" cargo-process-run "run")
     ("C" cargo-hydra/body "cargo...")))))

;; FIXME: Does lsp-sourcekit work with eglot, or is it lsp-mode specific?
;;        Also see:
;;          - https://github.com/danielmartin/swift-helpful/issues/2
;;          - https://www.reddit.com/r/emacs/comments/sndriv/i_finally_got_full_autocompetion_in_swift_with/
;;
;; (use-package lsp-sourcekit
;;   :after lsp-mode
;;   :config
;;   (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :defer t)

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
  (setq-default typescript-indent-level 2)
  :hook
  (typescript-mode . add-node-modules-path)
  (typescript-mode . prettier-js-mode)
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

;; Major mode for fish shell scripts.
;;
;; https://github.com/emacsmirror/fish-mode
(use-package fish-mode
  :defer t)

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; LSP-related Hydras. Adapted from:
;; https://github.com/jerrypnz/.emacs.d/blob/master/lisp/jp-lsp-hydra.el
;;
;; (major-mode-hydra-define+ (rust-mode swift-mode typescript-mode)
;;   (:color teal :quit-key "q")
;;   ("LSP Quick Action"
;;    (("d" lsp-describe-thing-at-point "describe symbol")
;;     ("a" lsp-execute-code-action "code action")
;;     ("f" lsp-format-buffer "format")
;;     ("O" lsp-organize-imports "organize imports"))
;;    "Find & Goto"
;;    (("gr" lsp-ui-peek-find-references "references")
;;     ("gd" lsp-ui-peek-find-definitions "definitions")
;;     ;; ("gs" consult-lsp-symbols "workspace symbol")
;;     ;; ("gf" consult-lsp-file-symbols "file symbol")
;;     ;; ("gx" consult-lsp-diagnostics "diagnostics")
;;     )
;;    "Connection"
;;    (("cc" lsp "start")
;;     ("cr" lsp-restart-workspace "restart")
;;     ("cd" lsp-describe-session "describe session")
;;     ("cq" lsp-shutdown-workspace "shutdown"))
;;    "Toggles"
;;    (("ol" lsp-lens-mode "toggle lens" :toggle t :exit nil)
;;     ("od" lsp-ui-doc-mode "toggle hover doc" :toggle t :exit nil)
;;     ("os" lsp-ui-sideline-mode "toggle sideline" :toggle t :exit nil))))
