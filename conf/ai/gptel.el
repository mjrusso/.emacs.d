;; gptel: https://github.com/karthink/gptel

(use-package gptel
  :config

  (defvar my/gptel-backend-openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :key (getenv "OPENROUTER_API_KEY")
      :models '(google/gemini-2.5-pro
                anthropic/claude-sonnet-4)))

  (defvar my/gptel-backend-openrouter-extras
    (gptel-make-openai "OpenRouter (Extras)"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :key (getenv "OPENROUTER_API_KEY")
      :models '(google/gemini-2.5-flash
                anthropic/claude-opus-4
                x-ai/grok-3
                x-ai/grok-3-mini)))

  (defvar my/gptel-backend-ollama
    (gptel-make-ollama "Ollama"
      :host "localhost:11434"
      :stream t
      :models '(mistral:latest
                zephyr:latest
                wizardcoder:33b
                deepseek-r1:70b)))

  (defvar my/gptel-backend-anthropic
    (gptel-make-anthropic "Anthropic" :key (getenv "ANTHROPIC_API_KEY")))

  (defvar my/gptel-backend-openai (gptel-make-openai "OpenAI"))

  (defvar my/gptel-backends-list
    `(("OpenRouter"          . ,my/gptel-backend-openrouter)
      ("OpenRouter (Extras)" . ,my/gptel-backend-openrouter-extras)
      ("Ollama"              . ,my/gptel-backend-ollama)
      ("Anthropic"           . ,my/gptel-backend-anthropic)
      ("OpenAI"              . ,my/gptel-backend-openai)))

  (defun my/gptel-select-default-backend ()
    "Select a gptel backend from a predefined list and set it as the default."
    (interactive)
    (let* ((backend-name (completing-read "Select backend: " my/gptel-backends-list nil t))
           (backend (cdr (assoc backend-name my/gptel-backends-list))))
      (when backend
        (setq gptel-backend backend)
        (message "gptel default backend set to: %s" backend-name))))

  (setq gptel-stream t
        gptel-display-buffer-action '(pop-to-buffer-same-window)
        gptel-api-key (getenv "OPENAI_API_KEY")
        gptel-model 'anthropic/claude-sonnet-4
        gptel-backend my/gptel-backend-openrouter)

  :bind
  (("C-c a i" . #'gptel)))
