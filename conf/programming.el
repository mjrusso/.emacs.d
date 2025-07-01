;; https://github.com/joaotavora/eglot
;;
;; Emacs LSP client.
;;
;; Some major modes are configured to start Eglot automatically. In cases where
;; Eglot is not auto-enabled, run `M-x eglot' to start it manually.
;;
;; Also note that `C-h .' is mapped to `eglot-help-at-point', but this does not
;; display diagnostic errors at point (only the documentation, if available).
;; It seems like this will be addressed in eldoc (see
;; https://github.com/joaotavora/eglot/issues/454), but for now, to see
;; diagnostic errors, either manually invoke `M-x display-local-help' or use
;; flymake (`M-x consult-flymake' is particularly nice).

(use-package eglot

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

;;https://www.flycheck.org/
(use-package flycheck
  :init
  (setq flycheck-indication-mode 'left-margin)
  (setq flycheck-highlighting-mode 'sexp)
  :hook (emacs-lisp-mode . flycheck-mode)
  :bind
  (:map flycheck-mode-map
        ("M-n" . flycheck-next-error)
        ("M-p" . flycheck-previous-error))
  )

;; https://github.com/flycheck/flycheck-eglot
(use-package flycheck-eglot
  :defer t
  :after (eglot flycheck)
  :hook ((eglot-managed-mode . flycheck-eglot-mode)))

;; Wingman: LLM-assisted text completion
;;
;; Requires running llama.cpp server running a model that supports FIM
;; (fill-in-the-middle) completion.
;;
;; https://github.com/mjrusso/wingman
(use-package wingman
  :straight (:type git :host github :repo "mjrusso/wingman")
  :ensure t
  :defer t

  :init

  (setq wingman-prefix-key (kbd "C-c a"))

  :hook (prog-mode . wingman-mode)

  :bind

  (:map wingman-mode-prefix-map
        ("z" . wingman-fim-inline)
        ("x" . wingman-debug-completion)
        :map wingman-mode-completion-transient-map
        ("TAB" . wingman-accept-full))

  :config

  (setq wingman-log-level 4)
  (setq wingman-key-accept-full (kbd "TAB"))
  (setq wingman-llama-endpoint "http://127.0.0.1:8012/infill")
  (setq wingman-ring-n-chunks 0)

  ;; assumes use of Modus Themes; substitute with preferred color scheme
  (set-face-attribute 'wingman-overlay-face nil
                      :foreground  (modus-themes-get-color-value 'red-warmer)
                      :background  (modus-themes-get-color-value 'bg-inactive))

  ;; don't provide completions in files that typically contain secrets
  (add-to-list 'wingman-disable-predicates
               (lambda ()
                 (or (derived-mode-p 'envrc-file-mode)
                     (derived-mode-p 'direnv-envrc-mode)
                     (when buffer-file-name
                       (let ((fname (file-name-nondirectory buffer-file-name)))
                         (or (string-equal ".env" fname)
                             (string-equal ".envrc" fname)
                             (string-prefix-p ".localrc" fname))))))))

;; Enable a `consulting-read' interface for eglot (specifically, the
;; `workspace/symbols` LSP procedure call).
;;
;; - https://github.com/mohkale/consult-eglot
(use-package consult-eglot
  :after (eglot consult)
  :bind (:map eglot-mode-map ("C-c a s" . #'consult-eglot-symbols)))

(use-package idle-highlight-mode
  :config
  (setq idle-highlight-exclude-point t)
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added (modus-themes-get-color-value 'green-warmer))
     (set-face-foreground 'diff-removed (modus-themes-get-color-value 'red-cooler))))

;; Highlight uncommitted changes in the gutter (renders to the fringe when in
;; the GUI app, and the margin when in the terminal).
;;
;; - https://github.com/dgutov/diff-hl
;;
;; See discussion about sharing the margin for diff-hl and flycheck indicators in TTY Emacs here:
;; https://github.com/mjrusso/.emacs.d/commit/5e19fc072124a1b5e65fb3d4aad969031d53a626#r151027896
(use-package diff-hl
  :config
  (setq diff-hl-margin-symbols-alist
        '((insert . " ")
          (delete . " ")
          (change . " ")
          (unknown . " ")
          (ignored . " ")))
  (global-diff-hl-mode)
  (if (not (display-graphic-p))
      (diff-hl-margin-mode)))

(use-package diff-hl
  :defer t
  :after (magit)
  :hook
  ((dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

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

;; Ensure that code is indented, even when making disruptive changes.
;; - https://github.com/Malabarba/aggressive-indent-mode
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'nix-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'fish-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'elixir-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'elixir-ts-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'heex-ts-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'swift-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'terraform-mode))

(use-package compile
  :config
  ;; Keep scrolling the compilation buffer as output appears. (Alternatively,
  ;; set to 'first-error to stop scrolling at the first error.)
  (setq compilation-scroll-output t)

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

  :bind
  (("C-c g" . magit-status))

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
  (setq magit-log-section-commit-count 40)

  ;; The magit status buffer sections are controlled by
  ;; `magit-status-sections-hook'. By default, it includes
  ;; `magit-insert-unpushed-to-upstream-or-recent', which displays the unpushed
  ;; commits OR the recent commits. Instead, always display the recent commits
  ;; section (in addition to the unpushed commits section).
  ;;
  ;; (1) Replace the default `magit-insert-unpushed-to-upstream-or-recent' with
  ;; the dedicated upstream commits section...
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-unpushed-to-upstream
   'magit-insert-unpushed-to-upstream-or-recent
   'replace)

  ;; (2) Add the dedicated recent commits section...
  (magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-recent-commits
   (car (last magit-status-sections-hook)) ;; Insert at the end.
   t)

  ;; (3) Because the default `magit-insert-unpushed-to-upstream-or-recent' is
  ;; removed, the transient actions `pu` is no longer valid. We have to replace
  ;; the action to jump to the dedicated upstream commits section.
  (transient-replace-suffix 'magit-status-jump "pu"
    '("pu" "Unmerged into upstream" magit-jump-to-unpushed-to-upstream
      :if (lambda ()
            (memq 'magit-insert-unpushed-to-upstream
                  magit-status-sections-hook))))
  )

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

(use-package cargo)

;; To use LSP with Rust, ensure rust-analyzer is installed:
;; https://rust-analyzer.github.io
;; To install, run: `rustup component add rust-analyzer`
(use-package rust-mode
  :defer t
  :init
  (setq rust-format-on-save t)
  :hook
  (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  (rust-mode . eglot-ensure)
  :config
  ;; Eventually, this won't be necessary; see
  ;; https://github.com/rust-lang/rustup/issues/2411 for details.
  (add-to-list 'eglot-server-programs
               ;; `rustup which --toolchain stable rust-analyzer`
               '(rust-mode "/Users/mjrusso/.rustup/toolchains/stable-x86_64-apple-darwin/bin/rust-analyzer")))

;; Syntax highlighting for WebGPU Shading Language (WGSL)
;; https://github.com/acowley/wgsl-mode
(use-package wgsl-mode
  :defer t)

;; https://github.com/swift-emacs/swift-mode
(use-package swift-mode
  :defer t
  :hook
  (swift-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")))
  )

;; Helper functions for working with Xcode (the GUI app, *not* the xcodebuild
;; command-line tool).
;;
;; Adapted from https://www.danielde.dev/blog/emacs-for-swift-development and
;; https://github.com/konrad1977/emacs/blob/main/localpackages/xcode-build.el
;;
;; IMPORTANT: these commands tell Xcode to build, run, or test the frontmost
;; project (i.e., whatever project is currently "active" in Xcode).
;;
;; TODO: add versions of these helper functions that use the xcodebuild
;; command-line tool, plugging in to the existing compile infrastructure; for
;; example: `(compile "xcodebuild -configuration Debug")'.

(defun my/xcode-build ()
  "Build with Xcode.

Note that this builds the project corresponding to the frontmost
Xcode window, which is not necessarily the same as the project
being edited in Emacs. Xcode must already be running for this
command to have any effect."
  (interactive)
  (save-some-buffers)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'build targetProject' -e 'end tell'")
  (message "Building frontmost project using Xcode..."))

(defun my/xcode-run ()
  "Run with Xcode.

Note that this builds the project corresponding to the frontmost
Xcode window, which is not necessarily the same as the project
being edited in Emacs. Xcode must already be running for this
command to have any effect."
  (interactive)
  (save-some-buffers)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'run targetProject' -e 'end tell'")
  (message "Running frontmost project using Xcode..."))

(defun my/xcode-test ()
  "Test with Xcode.

Note that this builds the project corresponding to the frontmost
Xcode window, which is not necessarily the same as the project
being edited in Emacs. Xcode must already be running for this
command to have any effect."
  (interactive)
  (save-some-buffers)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'test targetProject' -e 'end tell'")
  (message "Testing frontmost project using Xcode..."))

(defun my/xcode-stop ()
  "Tell Xcode to stop running the active operation.

Note that this builds the project corresponding to the frontmost
Xcode window, which is not necessarily the same as the project
being edited in Emacs. Xcode must already be running for this
command to have any effect."
  (interactive)
  (shell-command-to-string
   "osascript -e 'tell application \"Xcode\"' -e 'set targetProject to active workspace document' -e 'stop targetProject' -e 'end tell'")
  (message "Telling Xcode to stop..."))

;; https://github.com/danielmartin/swift-helpful
(use-package swift-helpful
  ;; Disabled until support for eglot is added: https://github.com/danielmartin/swift-helpful/issues/2
  :disabled)

;; https://github.com/prettier/prettier-emacs
(use-package prettier-js :defer t)

(use-package typescript-mode
  :defer t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode))
  :init
  (setq-default typescript-indent-level 2)
  :hook
  (typescript-mode . prettier-js-mode)
  (typescript-mode . eglot-ensure)
  (typescript-mode . (lambda () (indent-tabs-mode -1)))
  (typescript-mode . (lambda ()
                       ;; Don't apply formatting; we use Prettier instead.
                       (setq-local eglot-ignored-server-capabilities
                                   '(:documentFormattingProvider
                                     :documentRangeFormattingProvider))))
  :custom
  (js-indent-level 2)
  (js-jsx-indent-level 2)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(typescript-mode . ("typescript-language-server" "--stdio")))))

(use-package js2-mode
  :defer t
  :init
  (setq js-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . js2-jsx-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
  :config
  ;; Ensure that `M-.' is bound to `xref-jump-to-definition' instead of
  ;; `js2-jump-to-definition'.
  (define-key js2-mode-map "\M-." nil)
  :hook
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

(use-package python
  :config
  :hook
  (python-mode . eglot-ensure)
  )

;; https://github.com/wbolster/emacs-python-black
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; Tree-Sitter
;;
;; See [0] for an overview of Tree-Sitter support in Emacs.
;;
;; Language grammars are installed on my system [1] via the
;; `treesit-grammars.with-all-grammars` package [2].
;;
;; [0]: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; [1]: https://github.com/mjrusso/nixos-config/blob/main/overlays/emacs.nix
;; [2]: https://github.com/NixOS/nixpkgs/tree/master/pkgs/applications/editors/emacs/elisp-packages/manual-packages/treesit-grammars

;; Elixir
;;
;; Additional reference: https://medium.com/@victor.nascimento/elixir-emacs-development-2023-edition-1a6ccc40629
;;
;; Note that there are currently three competing language server options:
;;
;; - Next LS: https://github.com/elixir-tools/next-ls
;; - Lexical: https://github.com/lexical-lsp/lexical
;; - ElixirLS: https://github.com/elixir-lsp/elixir-ls

;; https://github.com/elixir-editors/emacs-elixir
(use-package elixir-mode
  :defer t

  :hook
  (elixir-mode . eglot-ensure)
  (before-save . eglot-format)

  :config
  (add-to-list 'eglot-server-programs
               `(elixir-mode . ,(eglot-alternatives
                                 `("elixir-ls" ("nextls" "--stdio=true") "lexical")))))

;; https://github.com/wkirschbaum/elixir-ts-mode
;;
;; (Included by default with Emacs 30+)
;;
;; Requires installation of the tree-sitter grammars; see comment above.
(use-package elixir-ts-mode
  :straight nil
  :defer t

  :disabled

  :mode ("\\.elixir\\'"
         "\\.ex\\'"
         "\\.exs\\'"
         "mix\\.lock")

  :hook
  (elixir-ts-mode . eglot-ensure)
  (before-save . eglot-format)

  :config
  (add-to-list 'eglot-server-programs
               `(elixir-ts-mode . ,(eglot-alternatives
                                    `("elixir-ls" ("nextls" "--stdio=true") "lexical")))))

;; https://github.com/wkirschbaum/heex-ts-mode
;;
;; (Included by default with Emacs 30+)
;;
;; Requires installation of the tree-sitter grammars; see comment above.
(use-package heex-ts-mode
  :defer t

  :mode ("\\.[hl]?eex\\'")

  :hook
  (heex-ts-mode . eglot-ensure)
  (before-save . eglot-format)

  :config
  (add-to-list 'eglot-server-programs
               `(heex-ts-mode . ,(eglot-alternatives
                                  `("elixir-ls" ("nextls" "--stdio=true") "lexical")))))

;; ExUnit test runner for Elixir.
;;
;; To run tests:
;;
;; `C-c , v' (`exunit-verify'):        run all tests in the current buffer/file
;;
;; `C-c , a' (`exunit-verify-all'):    run all tests in the current project
;;
;; `C-c , s' (`exunit-verify-single'): run the test under the point
;;
;; `C-c , r' (`exunit-rerun'):         re-run the last test invocation
;;
;; Also:
;;
;; `C-c , t' (`exunit-toggle-file-and-test'): toggle b/w a file and its tests
;;
;; https://github.com/ananthakumaran/exunit.el
(use-package exunit
  :custom  (transient-default-level 5)
  :config
  (add-hook 'elixir-mode-hook 'exunit-mode)
  (add-hook 'elixir-ts-mode-hook 'exunit-mode)
  )

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package clojure-mode
  :mode ("\\.clj\\'"
         "\\.cljs\\'"
         "\\.edn\\'"))

(use-package kotlin-ts-mode
  :mode "\\.kt\\'")

;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

;; https://github.com/leon-barrett/just-mode.el
(use-package just-mode)

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

;; https://github.com/john2x/jenkinsfile-mode
(use-package jenkinsfile-mode
  :mode "Jenkinsfile\\'")

;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode "Dockerfile\\'")

;; https://github.com/purcell/emacs-hcl-mode
(use-package hcl-mode
  :mode "\\.nomad\\'")

;; https://github.com/emacsorphanage/terraform-mode
(use-package terraform-mode
  :init
  (add-hook 'terraform-mode-hook
            (lambda () (add-hook 'before-save-hook 'terraform-format-buffer nil t))))

;; https://github.com/NixOS/nix-mode
(use-package nix-mode
  :mode "\\.nix\\'")
