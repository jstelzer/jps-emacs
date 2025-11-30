;;; jps-completion.el --- Code completion and quality tools -*- lexical-binding: t -*-
;;; Commentary:
;; Flycheck for syntax checking
;; Yasnippet for snippets
;; Company for completion
;;; Code:

(require 'use-package)

;;; ============================================================================
;;; Code Quality & Completion
;;; ============================================================================

(use-package flycheck :straight t :init (global-flycheck-mode))

(use-package yasnippet :straight t :config (yas-global-mode))

(use-package company
  :straight t
  :defer 0.1
  :config
  (global-company-mode t)
  (setq company-idle-delay 0.05
        company-require-match nil
        company-minimum-prefix-length 0
        company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)))

(provide 'jps-completion)
;;; jps-completion.el ends here
