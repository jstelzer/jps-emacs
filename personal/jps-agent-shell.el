;;; jps-agent-shell.el --- Agent-shell integration for LLM-powered terminal -*- lexical-binding: t -*-
;;; Commentary:
;; Integration for agent-shell (xenodium/agent-shell)
;; Provides LLM-powered shell with multiple backend support:
;; - Claude Code (Anthropic)
;; - Gemini CLI (Google)
;; - OpenAI Codex
;; - Goose CLI
;; - Cursor Agent
;; - Qwen Code
;;
;; This replaces vterm for AI-assisted development workflows.
;;; Code:

(require 'use-package)

;;; ============================================================================
;;; Agent Shell Configuration
;;; ============================================================================

;; Shell-maker - required dependency for agent-shell
;; Pinned to v0.84.1+ (required for agent-shell compatibility)
(use-package shell-maker
  :straight (shell-maker :type git :host github :repo "xenodium/shell-maker"
                         :ref "v0.84.1")
  :demand t)

;; ACP (Anthropic Claude Protocol) - required dependency for agent-shell
(use-package acp
  :straight (acp :type git :host github :repo "xenodium/acp.el")
  :demand t)

(use-package agent-shell
  :straight (agent-shell :type git :host github :repo "xenodium/agent-shell")
  :demand t
  :after acp
  :config
  ;; Configure Claude Code (Anthropic) backend
  ;; Uses login-based authentication by default
  ;; For API key auth, uncomment and set:
  ;; (setq agent-shell-anthropic-authentication
  ;;   (agent-shell-anthropic-make-authentication :api-key (getenv "ANTHROPIC_API_KEY")))

  ;; Configure environment variables (inherit system env)
  (setq agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         :inherit-env t))

  ;; Enable file capabilities for inserting files into agent conversations
  (setq agent-shell-text-file-capabilities t)

  ;; Buffer display configuration (same window by default)
  (setq agent-shell-display-action '(display-buffer-same-window))

  ;; Set Claude Code as preferred agent
  (setq agent-shell-preferred-agent-config
        (agent-shell-anthropic-make-claude-code-config)))

;;; ============================================================================
;;; Custom Commands & Keybindings
;;; ============================================================================

(defun jps-agent-shell ()
  "Start or switch to agent-shell buffer."
  (interactive)
  (agent-shell))

(defun jps-agent-shell-new ()
  "Start a new agent-shell session (with prefix arg behavior)."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'agent-shell)))

(defun jps-agent-shell-send-region (beg end)
  "Send region BEG..END to agent-shell."
  (interactive "r")
  (let ((text (buffer-substring-no-properties beg end)))
    (with-current-buffer (agent-shell)
      (goto-char (point-max))
      (insert text)
      (comint-send-input))))

(defun jps-agent-shell-send-buffer ()
  "Send entire buffer to agent-shell."
  (interactive)
  (jps-agent-shell-send-region (point-min) (point-max)))

;; Define C-c a as a prefix key for agent-shell commands
(define-prefix-command 'jps-agent-shell-map)
(global-set-key (kbd "C-c a") 'jps-agent-shell-map)

;; Global keybindings for agent-shell
(define-key jps-agent-shell-map (kbd "s") #'jps-agent-shell)
(define-key jps-agent-shell-map (kbd "n") #'jps-agent-shell-new)
(define-key jps-agent-shell-map (kbd "r") #'jps-agent-shell-send-region)
(define-key jps-agent-shell-map (kbd "b") #'jps-agent-shell-send-buffer)
(define-key jps-agent-shell-map (kbd "f") #'agent-shell-insert-file)
(define-key jps-agent-shell-map (kbd "c") #'agent-shell-insert-shell-command-output)

;; Specific backend launchers
(define-key jps-agent-shell-map (kbd "C") #'agent-shell-anthropic-start-claude-code)
(define-key jps-agent-shell-map (kbd "G") #'agent-shell-google-start-gemini)

(provide 'jps-agent-shell)
;;; jps-agent-shell.el ends here
