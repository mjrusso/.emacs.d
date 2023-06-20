;; Set default font to Berkeley Mono.
;;
;; - https://berkeleygraphics.com/typefaces/berkeley-mono/
;;
;; Note that, previously, I used Source Code Pro as my default font:
;;
;; - https://github.com/adobe/Source-Code-Pro
;; - http://blogs.adobe.com/typblography/2012/09/source-code-pro.html
;;
;; (add-to-list 'default-frame-alist '(font . "Source Code Pro Medium"))

(add-to-list 'default-frame-alist '(font . "Berkeley Mono"))

;; ligature.el: typographic ligatures
;;
;; - https://github.com/mickeynp/ligature.el
;; - https://github.com/mickeynp/ligature.el/wiki#berkeley-mono
(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode

   '(;; Group A
     ".." ".=" "..." "..<" "::" ":::" ":=" "::=" ";;" ";;;" "??" "???"
     ".?" "?." ":?" "?:" "?=" "**" "***" "/*" "*/" "/**"
     ;; Group B
     "<-" "->" "-<" ">-" "<--" "-->" "<<-" "->>" "-<<" ">>-" "<-<" ">->"
     "<-|" "|->" "-|" "|-" "||-" "<!--" "<#--" "<=" "=>" ">=" "<==" "==>"
     "<<=" "=>>" "=<<" ">>=" "<=<" ">=>" "<=|" "|=>" "<=>" "<==>" "||="
     "|=" "//=" "/="
     ;; Group C
     "<<" ">>" "<<<" ">>>" "<>" "<$" "$>" "<$>" "<+" "+>" "<+>" "<:" ":<"
     "<:<" ">:" ":>" "<~" "~>" "<~>" "<<~" "<~~" "~~>" "~~" "<|" "|>"
     "<|>" "<||" "||>" "<|||" "|||>" "</" "/>" "</>" "<*" "*>" "<*>" ":?>"
     ;; Group D
     "#(" "#{" "#[" "]#" "#!" "#?" "#=" "#_" "#_(" "##" "###" "####"
     ;; Group E
     "[|" "|]" "[<" ">]" "{!!" "!!}" "{|" "|}" "{{" "}}" "{{--" "--}}"
     "{!--" "//" "///" "!!"
     ;; Group F
     "www" "@_" "&&" "&&&" "&=" "~@" "++" "+++" "/\\" "\\/" "_|_" "||"
     ;; Group G
     "=:" "=:=" "=!=" "==" "===" "=/=" "=~" "~-" "^=" "__" "!=" "!==" "-~"
     "--" "---"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))


;; NOTE: consider using a package like https://github.com/WJCFerguson/textsize
;; to automatically adapt the font size based on the currently-active display.

(defun my/set-initial-font-size ()
  "Called by window-setup-hook to set my initial font height configuration."
  (my/set-font-size 160))

(add-hook 'window-setup-hook #'my/set-initial-font-size)

(defun my/set-font-size (new-height)
  (set-face-attribute 'default nil :height new-height)
  (message "Default font height set to %s" (face-attribute 'default :height)))

(defun my/zoom-text (arg)
  "Increase (or decrease) the default font size by the value of the universal argument ARG.
Note that if the absolute value of ARG is less than or equal to
4, the font is increased (or decreased) by 10 instead."
  (interactive "p")
  (let* ((current-height (face-attribute 'default :height))
         (absolute-delta (if (<= (abs arg) 4) 10 (abs arg)))
         (delta (if (natnump arg) absolute-delta (- absolute-delta)))
         (new-height (+ current-height delta)))
    (message "Current default font height is %s; changing (by %s)"
             current-height delta)
    (my/set-font-size new-height)))

(defun my/pick-text-zoom-level (arg)
  "Select a text size from a list of existing options."
  (interactive
   (list
    (completing-read "Choose text size: "
                     '("extra-small" "small" "default" "medium" "large" "extra-large"))))
  (cond ((string-equal arg "extra-small") (my/set-font-size 100))
         ((string-equal arg "small") (my/set-font-size 110))
         ((string-equal arg "default") (my/set-font-size 120))
         ((string-equal arg "medium") (my/set-font-size 160))
         ((string-equal arg "large") (my/set-font-size 180))
         ((string-equal arg "extra-large") (my/set-font-size 220))))

(defun my/zoom-text-bigger ()
  "Increase the default font size by 10 points."
  (interactive)
  (my/zoom-text 10))

(defun my/zoom-text-smaller ()
  "Decrease the default font size by 10 points."
  (interactive)
  (my/zoom-text -10))

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
