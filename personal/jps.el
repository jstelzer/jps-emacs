;;; jps.el --- Personal Emacs configuration (modular) -*- lexical-binding: t -*-
;;; Commentary:
;; Modular configuration - each module is loaded from personal/
;; This file orchestrates loading of all configuration modules
;;
;; Module structure:
;; - jps-core: Foundation utilities (no dependencies)
;; - jps-completion: Code completion and quality tools
;; - jps-lsp: Eglot LSP base configuration
;; - jps-ui: Modern UI packages (vertico, consult, etc.)
;; - jps-tools: External development tools
;; - jps-lang-*: Language-specific configurations
;; - jps-debug: Debug Adapter Protocol (DAP)
;; - jps-rest: REST/API development tools
;; - jps-project: Smart project management
;; - jps-ai: AI/LLM integration
;;
;; To disable a module, simply comment out its (require ...) line below.
;;; Code:

;;; ============================================================================
;;; Core Layer - Foundation (no dependencies)
;;; ============================================================================

(require 'jps-core)        ; Core utilities & settings
(require 'jps-completion)  ; Company, flycheck, yasnippet

;;; ============================================================================
;;; IDE Layer - Development Environment
;;; ============================================================================

(require 'jps-lsp)         ; Eglot base (needs jps-core)
(require 'jps-ui)          ; Vertico, consult, embark, treemacs, which-key
(require 'jps-agent-shell) ; Agent-shell LLM integration
(require 'jps-tools)       ; Magit, git-gutter, docker, etc.

;;; ============================================================================
;;; Language Support Layer
;;; ============================================================================
;; Note: These can be commented out if you don't work with a specific language

(require 'jps-lang-go)     ; Go support (go-mode, gopls, staticcheck, structlayout)
(require 'jps-lang-rust)   ; Rust support (rust-mode, cargo, rust-analyzer)
(require 'jps-lang-python) ; Python support (pyenv, blacken, pylsp, ruff)

;;; ============================================================================
;;; Debugging Layer
;;; ============================================================================

(require 'jps-debug)       ; DAP for Go, Rust, Python (requires language modules)

;;; ============================================================================
;;; Productivity Layer - Optional Tools
;;; ============================================================================

(require 'jps-rest)        ; REST/API development (restclient, jq, graphql)
(require 'jps-project)     ; Smart project commands (test/build/deploy)
(require 'jps-notes)       ; Org-roam knowledge management
(require 'jps-ai)          ; ChatGPT shell integration
(require 'jps-operator)    ; Stoic Operator/personal-cog work
;;; ============================================================================
;;; Configuration Complete
;;; ============================================================================

;; Log successful load
(message "JPS configuration loaded successfully (modular)")

(provide 'jps)
;;; jps.el ends here
