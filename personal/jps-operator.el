;;; jps-operator.el --- Stoic Operator integration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Glue layer to hook stoic-operator.el into my config:
;; - enables stoic-operator-mode globally
;; - sets any machine-specific defaults

;;; Code:

(require 'stoic-operator)

;; Choose your backend and model
;; (setq stoic-operator-backend 'ollama
;;       stoic-operator-ollama-model "mistral:7b-instruct-v0.2-q6_K")

(setq stoic-operator-backend 'ollama
      stoic-operator-ollama-model "llama3.1:8b")

;; Other ollama model options:
;; (setq stoic-operator-ollama-model "deepseek-r1:14b")
;; (setq stoic-operator-ollama-model "llama3.1:8b")

;; Or use OpenAI instead (uncomment and set backend to 'openai):
;; (setq stoic-operator-backend 'openai
;;       stoic-operator-command "openai"
;;       stoic-operator-model "gpt-4o")

(stoic-operator-mode 1)

(provide 'jps-operator)
;;; jps-operator.el ends here
