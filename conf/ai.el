;; Rundown of options for integrating ChatGPT into Emacs:
;; https://www.reddit.com/r/emacs/comments/11k1q0s/comment/jb64sgc/?context=3

;; GPTel: https://github.com/karthink/gptel
;;
;; Usage notes:
;;
;; *In a dedicated chat buffer:*
;;
;; - Run `M-x gptel' to start or switch to the ChatGPT buffer. Use a prefix-arg
;;   (`C-u M-x gptel') to start a new session.
;; - In the gptel buffer, send the prompt with M-x `gptel-send' (bound to
;;   `C-c RET' by default).
;; - Set chat parameters by calling `gptel-send' with a prefix argument (e.g.
;;   `C-u C-c RET').
;;
;; *In any buffer:*
;;
;; - Select a region of text, and call `M-x gptel-send'.
;; - The response will be inserted below your region. To continue the
;;   conversation, select both the original prompt and the response, and call
;;   `M-x gptel-send'.




(use-package gptel
  :config
  (setq gptel-api-key (string-trim (shell-command-to-string "echo $OPENAI_API_KEY")))

  :bind
  (("s-g" . #'gptel))
  )
