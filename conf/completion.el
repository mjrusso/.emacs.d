;; https://github.com/oantolin/orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides nil))

;; https://github.com/minad/corfu
;;
;; Includes configuration adapted from:
;; - https://kristofferbalintona.me/posts/corfu-kind-icon-and-corfu-doc/
(use-package corfu

  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-popupinfo))

  :custom

  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  (corfu-echo-documentation nil) ; Use corfu-popupinfo instead.
  (corfu-popupinfo-delay '(0.4 . 0.2))
  (corfu-popupinfo-width 70)
  (corfu-popupinfo-max-height 20)

  ;; Configuration for orderless completion. See
  ;; https://github.com/minad/corfu#orderless-completion for details.
  (corfu-quit-at-boundary t)
  (corfu-separator ?\s)
  (corfu-quit-no-match t)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)

  :init

  (global-corfu-mode)
  (corfu-popupinfo-mode)

  :config

  )

(use-package emacs
  :init

  ;; Never use completion cycling.
  (setq completion-cycle-threshold nil)

  ;; Disable Ispell completion function. (As an alternative, try `cape-dict'.)
  (setq text-mode-ispell-word-completion nil)

  ;; Works with `indent-for-tab-command', enabling both indentation and
  ;; completion with the TAB key.
  (setq tab-always-indent 'complete))

;; Use dabbrev with Corfu.
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;; https://github.com/jdtsmith/kind-icon
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (when (display-graphic-p)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
    ;; Ensure that the background colour is updated to match the active theme.
    (add-hook 'my/after-load-theme-hook #'kind-icon-reset-cache))
  )

;; https://github.com/minad/cape
(use-package cape
  :config
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;;
  ;; (add-hook 'completion-at-point-functions #'cape-dict)
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-file)
  )

;; https://github.com/minad/consult
;;
;; Config adapted from: https://github.com/minad/consult#use-package-example
(use-package consult
  :bind (
         ;; Custom C-c bindings
         ;;
         ;; C-c r: search entire project
         ;; C-u C-c r (or M-1 C-c r): allows user to specify search directory
         ;;
         ;; To display search results in a separate buffer, use Embark to
         ;; collect `consult-ripgrep` completion candidates in a collect buffer.
         ("C-c r" . consult-ripgrep)
         ("C-c F" . consult-find)
         ("C-c D" . my/consult-find-dir)
         ("C-c a o" . consult-outline)
         ("C-c a m" . consult-imenu)
         ("C-c a l" . consult-goto-line)
         ;; Note that `consult-line' is *not* intended as a replacement for
         ;; isearch; see: https://github.com/minad/consult/issues/417
         ("M-l" . consult-line)
         ("M-m" . consult-flycheck)

         ;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)

         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x r b" . consult-bookmark)
         ("C-x p b" . consult-project-buffer)

         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)

         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)

         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)

         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)

         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (not lazy).
  :init

  ;; Improve the register preview for `consult-register',
  ;; `consult-register-load', `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window: add thin lines, sorting and hide the
  ;; mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Find folders only (excluding files).
  ;;
  ;; From: https://emacs.stackexchange.com/a/68755
  (defun my/consult-find-dir ()
    "Search for regexp with find only DIR in DIR with INITIAL input."
    (interactive)
    (let ((consult-find-args "find . -type d -not ( -wholename */.* -prune )"))
       (consult-find)))

  (consult-customize
   consult-line :prompt "Search: ")

  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any))

  (consult-customize
   consult-buffer
   :preview-key nil)

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "<"))

  (consult-customize
   consult-find
   :sort t)

  (setq consult-narrow-key "<")

  )

(use-package consult-flycheck
  :after (flycheck consult))
