;;; jps.el --- Personal Emacs configuration (modular) -*- lexical-binding: t -*-
;;; Commentary:
;; Modular configuration - each module is loaded from personal/
;; This file orchestrates loading of all configuration modules, in the
;; order given by `jps-modules'.
;;
;; To disable modules on a machine, set `jps-disabled-modules' in
;; ~/.emacs.d/custom.el (loaded before this file):
;;
;;   (setq jps-disabled-modules '(jps-notes jps-hcl))
;;
;; Per-project behaviour is not configured here; see `jps-project-local'
;; in jps-core.el and the "Per-project settings" section of README.md.
;;; Code:

(defconst jps-modules
  '(;; Core layer - foundation (no dependencies)
    jps-core          ; Core utilities & settings (always loaded)
    jps-op            ; 1Password secrets (op CLI) + auth-source backend
    jps-completion    ; Company, flycheck, yasnippet

    ;; IDE layer - development environment
    jps-lsp           ; Eglot base (needs jps-core)
    jps-ui            ; Vertico, consult, embark, treemacs, which-key
    jps-agent-shell   ; Cloud agents (Claude, Codex, Gemini) over ACP
    jps-tools         ; Magit, git-gutter, docker, etc.

    ;; Language support layer
    jps-lang-go       ; Go (go-mode, gopls, staticcheck, structlayout)
    jps-lang-rust     ; Rust (rust-mode, cargo, rust-analyzer)
    jps-lang-python   ; Python (uv, basedpyright, ruff)
    jps-swift         ; Swift (swift-mode, sourcekit-lsp, swift-format)

    ;; Debugging layer
    jps-debug         ; DAP for Go, Rust, Python

    ;; Productivity layer - optional tools
    jps-rest          ; REST/API development (restclient, jq, graphql)
    jps-db            ; Databases: clutch (PostgreSQL, SQLite)
    jps-project       ; Smart project commands (test/build/deploy)
    jps-notes         ; Org-roam knowledge management
    jps-ellama        ; Local LLMs: ellama + Ollama, per-host model profile
    stoic-operator    ; Writing prompts on the local chat model (pulls in jps-ellama)
    jps-hcl)          ; HCL, atlas/terraform support
  "Modules loaded at startup, in order.")

(defcustom jps-disabled-modules
  (unless (eq system-type 'darwin) '(jps-swift))
  "Modules from `jps-modules' to skip at startup.
Set per machine in ~/.emacs.d/custom.el.  `jps-core' is always loaded.
A module required by another enabled module (e.g. `jps-ellama' via
`stoic-operator') still loads."
  :type '(repeat symbol)
  :group 'convenience)

(defun jps--module-enabled-p (module)
  "Return non-nil if MODULE should load at startup."
  (or (eq module 'jps-core)
      (not (memq module jps-disabled-modules))))

(dolist (module jps-modules)
  (when (jps--module-enabled-p module)
    (require module)))

(dolist (module jps-disabled-modules)
  (unless (memq module jps-modules)
    (display-warning 'jps (format "Unknown module in `jps-disabled-modules': %s"
                                  module))))

(message "JPS configuration loaded (%d of %d modules)"
         (seq-count #'jps--module-enabled-p jps-modules)
         (length jps-modules))

(provide 'jps)
;;; jps.el ends here
