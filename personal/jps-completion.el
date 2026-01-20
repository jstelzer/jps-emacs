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

(use-package corfu
  :straight t
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)                 ; Enable auto completion
  (corfu-cycle t)                 ; Cycle through candidates
  (corfu-auto-delay 0.2)          ; Delay before popup
  (corfu-auto-prefix 2)           ; Min chars before popup
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil))    ; Don't preview current candidate

(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

(provide 'jps-completion)
;;; jps-completion.el ends here
