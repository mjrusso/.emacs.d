;; https://github.com/oantolin/orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless)))

;; https://github.com/minad/consult
;;
;; Config adapted from: https://github.com/minad/consult#use-package-example
(use-package consult
  :after (perspective projectile)
  :bind (
         ;; s- bindings (custom)
         ;;
         ;; s-r: search entire project
         ;; C-u s-r (or M-1 s-r): allows user to specify search directory
         ;;
         ;; To display search results in a separate buffer, invoke
         ;; `M-x projectile-ripgrep` directly, or, better yet, use Embark to
         ;; collect `consult-ripgrep` completion candidates in a collect buffer.
         ("s-r" . consult-ripgrep)
         ("s-F" . consult-find)
         ("s-D" . my/consult-find-dir)
         ("s-o" . consult-outline)

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
         ("M-g M-g" . consult-goto-line)
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
         ("M-s l" . consult-line)        ;; Note that `consult-line' is *not*
                                         ;; intended as a replacement for
                                         ;; isearch:
                                         ;; https://github.com/minad/consult/issues/417
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

  (setq consult-project-function (lambda (_) (projectile-project-root)))

  ;; Wire up the list of perspective-filtered buffers as a candidate source.
  ;; (These will show up first when calling `consult-buffer'.) See
  ;; https://github.com/minad/consult#multiple-sources for more details.
  (my/add-perspective-virtual-buffer-source-to-consult)

  ;; Find folders only (excluding files).
  ;;
  ;; From: https://emacs.stackexchange.com/a/68755
  (defun my/consult-find-dir ()
    "Search for regexp with find only DIR in DIR with INITIAL input."
    (interactive)
    (let ((consult-find-args "find . -type d -not ( -wholename */.* -prune )"))
       (consult-find)))

  (consult-customize
   ;; For `consult-line' change the prompt and specify multiple preview
   ;; keybindings. Note that you should bind <S-up> and <S-down> in the
   ;; `minibuffer-local-completion-map' or `vertico-map' to the commands which
   ;; select the previous or next candidate.
   consult-line :prompt "Search: "
   :preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

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
   :preview-key (kbd "M-."))

  (consult-customize
   consult-find
   :sort t)

  (setq consult-narrow-key "<")

  )
