;; https://github.com/rougier/svg-lib
(use-package svg-lib)

;; https://github.com/rougier/svg-tag-mode/
;;
;; Configuration adapted from:
;; https://github.com/rougier/svg-tag-mode/blob/main/examples/example-2.el
(use-package svg-tag-mode
  :disabled

  :config

  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
      (svg-image (svg-lib-concat
                  (svg-lib-progress-bar (/ count total) nil
                                        :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                  (svg-lib-tag value nil
                               :stroke 0 :margin 0)) :ascent 'center)))


  ;; To do:         TODO DONE
  ;; Progress:      [1/3]
  ;;                [42%]

  (setq svg-tag-tags
        `(
          ;; Progress
          ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
                                              (svg-progress-percent (substring tag 1 -2)))))
          ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
                                            (svg-progress-count (substring tag 1 -1)))))

          ;; TODO / DONE
          ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo :inverse t :margin 0))))
          ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))
          )
        )
  )
