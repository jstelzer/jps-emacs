;;; jps-tools.el --- External development tools integration -*- lexical-binding: t -*-
;;; Commentary:
;; Integrations for various development tools:
;; - agent-shell (LLM-powered terminal)
;; - magit (git interface)
;; - git-gutter (show git diff in gutter)
;; - deadgrep (fast grep)
;; - docker (container management)
;; - multiple-cursors (edit multiple locations)
;; - markdown-mode (markdown editing)
;; - claude-code-ide (AI assistant)
;;; Code:

(require 'use-package)

;;; ============================================================================
;;; External Tools Integration
;;; ============================================================================

;; Agent-shell is loaded via jps-agent-shell.el
;; Keep vterm as optional fallback for non-LLM terminal sessions
(use-package vterm :straight t :defer t)

(use-package magit :straight t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter :straight t
  :hook (prog-mode . git-gutter-mode)
  :config (setq git-gutter:update-interval 0.02))

(use-package deadgrep :straight t
  :bind ("C-x p G" . deadgrep))

(use-package docker :straight t :bind ("C-c d" . docker))

(use-package multiple-cursors :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package markdown-mode :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Ridiculous coding effects - make coding absurdly dramatic
;; Toggle: M-x ridiculous-coding-mode
;; Full chaos: M-x global-ridiculous-coding-mode
(use-package ridiculous-coding
  :straight (:type git :host github :repo "jstelzer/ridiculous-coding.el")
  :commands (ridiculous-coding-mode
             global-ridiculous-coding-mode
             ridiculous-coding-set-intensity))

(setq claude-code-vterm-buffer-multiline-output nil)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config (claude-code-ide-emacs-tools-setup))

(provide 'jps-tools)
;;; jps-tools.el ends here
