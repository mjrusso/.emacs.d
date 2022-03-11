;; Spellchecking, for prose and code comments.
;;
;; Requires a spellchecker to be installed. Aspell is a good choice. On Mac:
;;
;;     $ brew install aspell
;;
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html

(use-package ispell
  :init
  (setq ispell-personal-dictionary
        (concat user-emacs-directory "ispell-personal-dictionary")))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :init
  (setq flyspell-persistent-highlight t)
  :config
  ;; Make it possible to right-click on a word in order to choose a correction.
  ;; https://www.tenderisthebyte.com/blog/2019/06/09/spell-checking-emacs/
  (progn
    (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
    (define-key flyspell-mouse-map [mouse-3] #'undefined)))
