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
  (setq gptel-api-key (string-trim (shell-command-to-string "echo $OPENAI_API_KEY"))
        ;; gptel-model "gpt-4o"
        gptel-model 'claude-3-5-sonnet-20241022
        gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :key (string-trim
                              (shell-command-to-string "echo $ANTHROPIC_API_KEY")))
        gptel-display-buffer-action '(pop-to-buffer-same-window)

        )
  :bind
  (("C-c a i" . #'gptel))
  )
