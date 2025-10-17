;;; jps-lang-go.el --- Go language support -*- lexical-binding: t -*-
;;; Commentary:
;; Go development environment with:
;; - go-mode for editing
;; - gopls LSP server
;; - gofmt/goimports formatting
;; - staticcheck for linting
;; - structlayout tools for struct analysis
;; - transient menu for quick access
;;; Code:

(require 'use-package)
(require 'jps-core)

;;; ============================================================================
;;; Go Language Support
;;; ============================================================================

;; Go
(use-package go-mode
  :straight t
  :hook (before-save . gofmt-before-save))

(with-eval-after-load 'go-mode
  ;; Use goimports globally; Eglot organize imports too
  (setq gofmt-command "goimports"))

;; gopls tuning
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (setq-default eglot-workspace-configuration
                (append
                 '((gopls . ((usePlaceholders . t)
                             (completeUnimported . t)
                             (staticcheck . t)
                             (gofumpt . t)
                             (hints . ((assignVariableTypes . t)
                                       (compositeLiteralFields . t)
                                       (compositeLiteralTypes . t)
                                       (constantValues . t)
                                       (functionTypeParameters . t)
                                       (parameterNames . t)
                                       (rangeVariableTypes . t))))))
                 eglot-workspace-configuration)))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (bound-and-true-p eglot--managed-mode)
                          (ignore-errors (eglot-code-action-organize-imports))))
                      nil t)))

;;; ============================================================================
;;; Go Development Tools (staticcheck, structlayout)
;;; ============================================================================

;; External binaries required (install with go install):
;;   go install honnef.co/go/tools/cmd/staticcheck@latest
;;   go install honnef.co/go/tools/cmd/structlayout@latest
;;   go install honnef.co/go/tools/cmd/structlayout-pretty@latest
;;   go install honnef.co/go/tools/cmd/structlayout-optimize@latest

;; --- Staticcheck integration (Eglot-friendly; uses compilation-mode) ---
(defun jps-go-staticcheck-project ()
  "Run staticcheck ./... from the current project root."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (compile "staticcheck ./...")))

(defun jps-go-staticcheck-pkg ()
  "Run staticcheck . in the current buffer's package directory."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory "go.mod")
                               default-directory)))
    (compile "staticcheck .")))

;; Handy bindings in go-mode:
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-s p") #'jps-go-staticcheck-project)
  (define-key go-mode-map (kbd "C-c C-s .") #'jps-go-staticcheck-pkg))

;; --- Structlayout helpers ---
(defun jps--symbol-at-point-or-read (prompt)
  (let* ((sym (thing-at-point 'symbol t))
         (def (and sym (substring-no-properties sym))))
    (read-string prompt def)))

(defun jps-structlayout (type-name)
  "Display raw struct layout for TYPE-NAME using structlayout on the current buffer."
  (interactive (list (jps--symbol-at-point-or-read "Struct type: ")))
  (let ((buf (get-buffer-create (format "*structlayout %s*" type-name))))
    (with-current-buffer buf (erase-buffer))
    (call-process-region
     (point-min) (point-max) "structlayout" nil buf nil type-name)
    (pop-to-buffer buf)
    (special-mode)))

(defun jps-structlayout-pretty (type-name)
  "Display pretty ASCII layout for TYPE-NAME using structlayout | structlayout-pretty."
  (interactive (list (jps--symbol-at-point-or-read "Struct type: ")))
  (let ((tmp (generate-new-buffer " *structlayout-tmp*"))
        (out (get-buffer-create (format "*structlayout-pretty %s*" type-name))))
    (unwind-protect
        (progn
          (call-process-region (point-min) (point-max) "structlayout" nil tmp nil type-name)
          (with-current-buffer out (erase-buffer))
          (with-current-buffer tmp
            (call-process-region (point-min) (point-max) "structlayout-pretty" nil out))
          (pop-to-buffer out)
          (special-mode))
      (when (buffer-live-p tmp) (kill-buffer tmp)))))

(defun jps-structlayout-optimize (type-name)
  "Suggest optimal field ordering for TYPE-NAME via structlayout-optimize."
  (interactive (list (jps--symbol-at-point-or-read "Struct type: ")))
  (let ((buf (get-buffer-create (format "*structlayout-optimize %s*" type-name))))
    (with-current-buffer buf (erase-buffer))
    (call-process-region
     (point-min) (point-max) "structlayout-optimize" nil buf nil type-name)
    (pop-to-buffer buf)
    (special-mode)))

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-l l") #'jps-structlayout)
  (define-key go-mode-map (kbd "C-c C-l p") #'jps-structlayout-pretty)
  (define-key go-mode-map (kbd "C-c C-l o") #'jps-structlayout-optimize))

;; Usage:
;;   Put point on the struct name (or just type it when prompted).
;;   C-c C-l p → nice ASCII layout (padding visualized)
;;   C-c C-l o → optimal field reordering suggestion
;;   C-c C-l l → raw layout (useful for scripting/grepping)
;;
;; These pipe the current buffer into the tools, so you can even run them on unsaved edits.

;; Optional transient for quick access
(use-package transient :straight t)
(transient-define-prefix jps-go-tools-menu ()
  "Go Tools"
  [["Staticcheck"
    ("p" "project ./..." jps-go-staticcheck-project)
    ("." "package ."     jps-go-staticcheck-pkg)]
   ["Structlayout"
    ("l" "raw layout"    jps-structlayout)
    ("r" "pretty layout" jps-structlayout-pretty)
    ("o" "optimize"      jps-structlayout-optimize)]])

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-t") #'jps-go-tools-menu))
;; Hit C-c C-t in a Go buffer for a quick pop-up of all the above.

(provide 'jps-lang-go)
;;; jps-lang-go.el ends here
