;;; jps-lang-rust.el --- Rust language support -*- lexical-binding: t -*-
;;; Commentary:
;; Rust development environment with:
;; - rust-mode + cargo
;; - rust-analyzer (LSP)
;; - crates.el (Smart Cargo.toml editing)
;; - cargo-expand integration (Macro debugging)
;;; Code:

(require 'use-package)
(require 'jps-core)

;; Disable flycheck in rust-mode (eglot uses flymake)
(add-hook 'rust-mode-hook (lambda () (flycheck-mode -1)))
;; TOML support with Crates.io intelligence
(use-package toml-mode :straight t)

;;(use-package crates
 ;; :straight t
;;  :hook (toml-mode . crates-mode)
;;  :config
  ;; Smart feature: auto-enable semantic versioning assistance
;;  (setq crates-backend 'url-retrieve)) 

;;; ============================================================================
;;; Rust Mode & Cargo
;;; ============================================================================

(use-package rust-mode
  :straight t
  :init
  (setq rust-mode-treesitter-derive nil) ;; Use tree-sitter if available
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(use-package rust-playground :straight t)

;;; ============================================================================
;;; Advanced Tooling
;;; ============================================================================

;; External binaries required (install with cargo install):
;;   cargo install cargo-expand
;;   cargo install cargo-audit
(defun jps-rust-expand-macro ()
  "Run 'cargo expand' on the current file/module to see generated code.
Useful for debugging complex macros like Serde or async traits."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory))
         (buffer-name "*cargo-expand*"))
    (message "Running cargo expand...")
    (with-current-buffer (get-buffer-create buffer-name)
      (read-only-mode 0)
      (erase-buffer)
      ;; We try to expand the current module based on file name
      (let ((args (if (string-match-p "main.rs" (buffer-file-name))
                      ""
                    (concat " " (file-name-base (buffer-file-name))))))
        (start-process-shell-command "cargo-expand" (current-buffer)
                                     (concat "cargo expand" args)))
      (pop-to-buffer (current-buffer))
      (rust-mode) ;; Syntax highlight the output
      (special-mode))))

(defun jps-rust-audit ()
  "Run 'cargo audit' to check dependencies for known vulnerabilities."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (compile "cargo audit")))

;;; ============================================================================
;;; LSP Configuration (rust-analyzer)
;;; ============================================================================

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  
  (setq-default eglot-workspace-configuration
                (append
                 '((rust-analyzer
                    . ((cargo . ((allFeatures . t)
                                 (loadOutDirsFromCheck . t)))
                       ;;  Run Clippy, not just 'check'
                       (check . ((command . "clippy")
                                 (extraArgs . ["--tests"]))) 
                       (diagnostics . ((disabled . ["unresolved-proc-macro"])))
                       ;; Inlay Hints (The "Reading Glasses")
                       (inlayHints . ((bindingModeHints . t)
                                      (closingBraceHints . t)
                                      (closureReturnTypeHints . "always")
                                      (lifetimeElisionHints . "always")
                                      (parameterHints . t)
                                      (typeHints . t))))))
                 eglot-workspace-configuration)))

;; Organize imports on save (mirrors Go behavior)
(add-hook 'rust-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (bound-and-true-p eglot--managed-mode)
                          (ignore-errors (eglot-code-action-organize-imports))))
                      nil t)))

;;; ============================================================================
;;; Transient Menu (The "Stoic" Interface)
;;; ============================================================================

;; transient loaded in jps-core
(transient-define-prefix jps-rust-menu ()
  "Rust Commands"
  ["Build & Test"
   ("c" "Clippy"         cargo-process-clippy)
   ("C" "Check"          cargo-process-check)
   ("b" "Build"          cargo-process-build)
   ("r" "Run"            cargo-process-run)
   ("t" "Test (Project)" cargo-process-test)
   ("T" "Test (Current)" cargo-process-current-test)]
  ["Tools"
   ("e" "Expand Macro"   jps-rust-expand-macro)
   ("f" "Format"         rust-format-buffer)
   ("d" "Doc (Open)"     cargo-process-doc-open)
   ("u" "Update deps"    cargo-process-update)
   ("a" "Audit (vulns)"  jps-rust-audit)])

(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-t") #'jps-rust-menu))

(provide 'jps-lang-rust)
;;; jps-lang-rust.el ends here
