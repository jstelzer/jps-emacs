;;; jps-agent-shell.el --- ACP agents in Emacs -*- lexical-binding: t -*-
;;; Commentary:
;; Keep Claude as the preferred agent and use the locally installed cola-io
;; Codex ACP adapter for its built-in acp_fs tools.  Authentication uses CLI
;; login.  Region, buffer, and file commands stage context before sending.
;;; Code:

(require 'use-package)

;; Keep dependencies on their upstream branches alongside agent-shell.
(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker")
  :defer t)

(use-package acp
  :straight (:type git :host github :repo "xenodium/acp.el")
  :defer t)

(use-package agent-shell
  :straight (:type git :host github :repo "xenodium/agent-shell")
  :demand t
  :config
  (setq agent-shell-preferred-agent-config 'claude-code
        agent-shell-text-file-capabilities t)
  (require 'agent-shell-openai)
  (let ((command (list (expand-file-name "~/bin/codex-acp"))))
    (if (boundp 'agent-shell-openai-codex-acp-command)
        (setq agent-shell-openai-codex-acp-command command
              agent-shell-openai-codex-command nil)
      (setq agent-shell-openai-codex-command command)))
  (setq agent-shell-openai-codex-environment
        (cons "CODEX_LOG_STDERR=0"
              (seq-remove
               (lambda (entry)
                 (string-prefix-p "CODEX_LOG_STDERR=" entry))
               agent-shell-openai-codex-environment))))

(defun jps-agent-shell-new ()
  "Start a new agent-shell session."
  (interactive)
  (agent-shell '(4)))

(defun jps-agent-shell-send-buffer ()
  "Stage the accessible buffer text in agent-shell for review before sending."
  (interactive)
  (save-mark-and-excursion
    (goto-char (point-min))
    (push-mark (point-max) t t)
    (let ((transient-mark-mode t))
      (agent-shell-send-region))))

(define-prefix-command 'jps-agent-shell-map)
(global-set-key (kbd "C-c a") 'jps-agent-shell-map)

(define-key jps-agent-shell-map (kbd "s") #'agent-shell)
(define-key jps-agent-shell-map (kbd "n") #'jps-agent-shell-new)
(define-key jps-agent-shell-map (kbd "r") #'agent-shell-send-region)
(define-key jps-agent-shell-map (kbd "b") #'jps-agent-shell-send-buffer)
(define-key jps-agent-shell-map (kbd "f") #'agent-shell-send-file)

(define-key jps-agent-shell-map (kbd "C") #'agent-shell-anthropic-start-claude-code)
(define-key jps-agent-shell-map (kbd "O") #'agent-shell-openai-start-codex)
(define-key jps-agent-shell-map (kbd "G") #'agent-shell-google-start-gemini)

;; This command requires an agent-shell buffer.
(define-key agent-shell-mode-map (kbd "C-c a c")
            #'agent-shell-insert-shell-command-output)

(provide 'jps-agent-shell)
;;; jps-agent-shell.el ends here
