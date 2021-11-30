;; Org-Reveal: https://github.com/yjwen/org-reveal
;;
;; To export an org file as a presentation: `C-c C-e R R`.

(use-package ox-reveal
  :after org
  :init (setq org-reveal-root "file:///Users/mjrusso/Dropbox/Code/-Others/reveal.js"))
