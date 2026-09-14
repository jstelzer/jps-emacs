;;; jps-agent-shell.el --- ACP agents in Emacs -*- lexical-binding: t -*-
;;; Commentary:
;; Use agent-shell defaults, with Claude as the preferred agent.
;; Authentication defaults to CLI login; ACP file read/write support is enabled
;; by default.  Region, buffer, and file commands stage context before sending.
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
  (setq agent-shell-preferred-agent-config 'claude-code))

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
