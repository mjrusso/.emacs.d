;; Set default font to Source Code Pro.
;; - https://github.com/adobe/Source-Code-Pro
;; - http://blogs.adobe.com/typblography/2012/09/source-code-pro.html
(add-to-list 'default-frame-alist '(font . "Source Code Pro Medium"))

;; Display emojis in buffer, when Apple's emoji font is available.
(when (member "Apple Color Emoji" (font-family-list))
  (set-fontset-font
   t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; Search and insert emoji by name.
;; - https://github.com/iqbalansari/emacs-emojify
(use-package emojify
  :config
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  :bind ("C-c i e" . emojify-insert-emoji))
