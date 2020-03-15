;; Use visual line mode by default for org files.
(add-hook 'org-mode-hook 'turn-off-auto-fill)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Use org-indent-mode by default.
(add-hook 'org-mode-hook (lambda () (org-indent-mode t)) t)

(setq org-completion-use-ido t)
