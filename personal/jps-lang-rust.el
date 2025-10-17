;;; jps-lang-rust.el --- Rust language support -*- lexical-binding: t -*-
;;; Commentary:
;; Rust development environment with:
;; - rust-mode for editing
;; - cargo for building/testing
;; - rust-analyzer via Eglot (configured in jps-lsp.el)
;; - rust-playground for quick experiments
;;; Code:

(require 'use-package)

;;; ============================================================================
;;; Rust Language Support
;;; ============================================================================

;; Docker/YAML modes (commonly used with Rust projects)
(use-package dockerfile-mode :straight t)
(use-package yaml-mode       :straight t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\`Dockerfile\\'" . dockerfile-mode))

;; Rust
(use-package rust-mode :straight t)
(use-package cargo     :straight t :hook (rust-mode . cargo-minor-mode))
(use-package rust-playground :straight t)

(with-eval-after-load 'rust-mode
  (setq rust-format-on-save t))

;; rust-analyzer LSP configuration
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(rust-mode . ("rust-analyzer")))
  (setq-default eglot-workspace-configuration
                (append
                 '((rust-analyzer
                    . ((cargo . ((allFeatures . t)))
                       ;; ✅ modern setting: run clippy on save
                       (check . ((command . "clippy")))
                       ;; (optional) if you want to toggle on/off explicitly:
                       ;; (checkOnSave . t)
                       (imports . ((granularity . ((group . "module")))
                                   (prefix . "by_self")))
                       (inlayHints . ((bindingModeHints . t)
                                      (closingBraceHints . t)
                                      (closureReturnTypeHints . "always")
                                      (parameterHints . t)
                                      (typeHints . t))))))
                 eglot-workspace-configuration)))

(provide 'jps-lang-rust)
;;; jps-lang-rust.el ends here
