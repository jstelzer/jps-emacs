;;; jps-ai.el --- AI/LLM integration (ChatGPT) -*- lexical-binding: t -*-
;;; Commentary:
;; ChatGPT integration via chatgpt-shell
;; Provides chat interface and helpers for sending code/regions
;;; Code:

(require 'use-package)

;;; ============================================================================
;;; ChatGPT (chatgpt-shell) + vterm integration
;;; ============================================================================

;; Requires: env var OPENAI_API_KEY set in your shell (you already have one).
;;   export OPENAI_API_KEY="sk-...yourkey..."
;;
;; Optional: auth-source alternative is shown below.

(use-package chatgpt-shell
  :straight t
  :custom
  ;; Pull the key from environment (preferred; works with exec-path-from-shell)
  (chatgpt-shell-openai-key (lambda () (getenv "OPENAI_API_KEY")))
  ;; Pick a default model; tweak any time with `M-x customize-variable`
  (chatgpt-shell-model "gpt-4o-mini")
  ;; Keep replies tight by default; tune per buffer as needed
  (chatgpt-shell-system-prompt
   "You are a concise, code-savvy assistant. Prefer short, actionable replies.")
  :bind
  (("C-c g c" . chatgpt-shell)            ; open chat buffer
   ("C-c g r" . chatgpt-shell-send-region)
   ("C-c g b" . chatgpt-shell-send-buffer)))

;; Side-by-side: project vterm (left) + ChatGPT buffer (right)
(defun jps-chatgpt-vterm-and-chat ()
  "Open a vterm in the project root on the left and ChatGPT on the right."
  (interactive)
  (delete-other-windows)
  (call-interactively #'jps-project-vterm)  ;; you already defined this
  (split-window-right)
  (other-window 1)
  (chatgpt-shell)
  (other-window -1))

(global-set-key (kbd "C-c g v") #'jps-chatgpt-vterm-and-chat)

;; QoL: send region to ChatGPT but keep focus in the current window
(defun jps-chatgpt-send-region-stay (beg end)
  "Send region BEG..END to chatgpt-shell but keep point/window."
  (interactive "r")
  (let ((txt (buffer-substring-no-properties beg end)))
    (with-current-buffer (chatgpt-shell)
      (goto-char (point-max))
      (insert txt)
      (chatgpt-shell-submit))))

(global-set-key (kbd "C-c g s") #'jps-chatgpt-send-region-stay)

;; Optional: which-key hints for your new bindings
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "C-c g" "ChatGPT/LLM"))

;;; --- Optional: use auth-source (~/.authinfo.gpg) instead of env var ----------
;; Put this line (one time) into ~/.authinfo.gpg:
;;   machine api.openai.com login apikey password sk-yourkey
;; Then use the resolver below (and remove/override the env-based one above):
;;
;; (setq chatgpt-shell-openai-key
;;       (lambda ()
;;         (let* ((m (car (auth-source-search :host "api.openai.com"
;;                                            :user "apikey" :require '(:secret)))))
;;           (when m (funcall (plist-get m :secret))))))

;;; --- Optional: switch to a local OpenAI-compatible endpoint (e.g., Ollama) ---
;; If you run ollama with the OpenAI-compatible server, set:
;;   ollama serve --host 127.0.0.1 --port 11434
;; and enable its /v1 API. Then:
;;
;; (setq chatgpt-shell-openai-base-url "http://127.0.0.1:11434/v1"
;;       chatgpt-shell-model "qwen2.5:3b")  ;; or your pulled model name
;;
;; Toggle back to OpenAI API by clearing base-url:
;; (setq chatgpt-shell-openai-base-url nil
;;       chatgpt-shell-model "gpt-4o-mini")

(provide 'jps-ai)
;;; jps-ai.el ends here
