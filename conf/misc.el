;; Never trigger the alarm bell.
(setq ring-bell-function
      (lambda ()))

;; Do not scroll dramatically when moving only one line at a time.
(setq-default scroll-conservatively 1)

;; Enable cua mode, but only for rectangles.
;; (C-RET will start a rectangular selection.)
(setq cua-enable-cua-keys nil)
(cua-mode t)

(winner-mode)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; When you visit a file, point goes to the last place where it was when you
;; previously visited the same file.
;; - https://www.emacswiki.org/emacs/SavePlace
(save-place-mode 1)

;; Persist M-x command history across shutdowns.
;; - https://www.emacswiki.org/emacs/SaveHist
(savehist-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(unless (or (fboundp 'helm-mode) (fboundp 'ivy-mode))
  (ido-mode t)
  (setq ido-enable-flex-matching t))

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-faces nil
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-ubiquitous-allow-on-functional-collection t)

(require 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'counsel-M-x) ;; `counsel-M-x' is enhanced by smex.

(require 'imenu-list)

(require 'find-file-in-project)
(require 'find-things-fast)

;; Enable the menu bar.
(menu-bar-mode t)

;; Move point from window to window using shift and the arrow keys.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
