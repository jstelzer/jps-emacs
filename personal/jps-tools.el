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
;; Keep ghostel as optional fallback for non-LLM terminal sessions
;; (libghostty-vt based; native module auto-downloads on first use)
(use-package ghostel :straight t :defer t)

(use-package magit :straight t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter :straight t
  :hook (prog-mode . git-gutter-mode)
  :config (setq git-gutter:update-interval 0.02))

(use-package deadgrep :straight t
  :bind ("C-x p G" . deadgrep))

(use-package docker :straight t :bind ("C-c D" . docker))

(use-package multiple-cursors :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package markdown-mode :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package terraform-mode :straight t
  :mode "\\.tf\\'")

;; Ridiculous coding effects - make coding absurdly dramatic
;; Toggle: M-x ridiculous-coding-mode
;; Full chaos: M-x global-ridiculous-coding-mode
(use-package ridiculous-coding
  :straight (:type git :host github :repo "jstelzer/ridiculous-coding.el"
             :files (:defaults "sounds" "images"))
  :commands (ridiculous-coding-mode
             global-ridiculous-coding-mode
             ridiculous-coding-set-intensity))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (setq claude-code-ide-terminal-backend 'ghostel)
  (claude-code-ide-emacs-tools-setup))

(use-package justl
  :ensure t)

(use-package just-mode
  :ensure t)

(use-package json-mode
  :ensure t)
;; Tilt mode.

;; (require 'python-mode)

;; (define-derived-mode tiltfile-mode
;;   python-mode "tiltfile"
;;   "Major mode for Tilt Dev."
;;   (setq-local case-fold-search nil))

;; (add-to-list 'auto-mode-alist '("Tiltfile$" . tiltfile-mode))

;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration
;;     '(tiltfile-mode . "tiltfile"))

;;   (lsp-register-client
;;     (make-lsp-client :new-connection (lsp-stdio-connection `("tilt" "lsp" "start"))
;;                      :activation-fn (lsp-activate-on "tiltfile")
;;                      :server-id 'tilt-lsp)))
(provide 'jps-tools)
;;; jps-tools.el ends here
