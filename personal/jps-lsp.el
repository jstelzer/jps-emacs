;;; jps-lsp.el --- LSP (Eglot) base configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Base configuration for Eglot LSP client
;; Language-specific configurations are in jps-lang-*.el files
;;; Code:

(require 'use-package)

;;; ============================================================================
;;; LSP via Eglot
;;; ============================================================================

;; Increase process output buffer size for LSP performance
(setq read-process-output-max (* 1024 1024)) ; 1MB

(use-package eglot
  :straight t
  :hook ((rust-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (python-mode . eglot-ensure)))

(with-eval-after-load 'eglot
  ;; Less chatty; better xref; faster change notifications
  (setq eglot-events-buffer-size 0
        eglot-extend-to-xref t
        eglot-send-changes-idle-time 0.1)

  ;; Prefer LSP (capf) completions for company
  (with-eval-after-load 'company
    (setq company-backends '(company-capf)))

  ;; Use Flymake in Eglot buffers; quiet Flycheck there; enable inlay hints (Emacs 29+)
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (bound-and-true-p flycheck-mode) (flycheck-mode -1))
              (flymake-mode 1)
              (when (fboundp 'eglot-inlay-hints-mode)
                (eglot-inlay-hints-mode 1))))

  ;; Format on save if server supports it
  (defun jps-eglot-format-on-save-maybe ()
    (when (and (bound-and-true-p eglot--managed-mode)
               (eglot--server-capable :documentFormattingProvider))
      (eglot-format-buffer)))
  (add-hook 'before-save-hook #'jps-eglot-format-on-save-maybe)

  ;; Handy keys
  (define-key eglot-mode-map (kbd "C-c e a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c e r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e o") #'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c e i") #'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c e t") #'eglot-find-typeDefinition))

(provide 'jps-lsp)
;;; jps-lsp.el ends here
