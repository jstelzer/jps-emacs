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
;;   go install golang.org/x/vuln/cmd/govulncheck@latest

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

;; --- Govulncheck integration (vulnerability scanning) ---
(defun jps-go-vulncheck-project ()
  "Run govulncheck ./... from the current project root."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (compile "govulncheck ./...")))

(defun jps-go-vulncheck-pkg ()
  "Run govulncheck . in the current buffer's package directory."
  (interactive)
  (let ((default-directory (or (locate-dominating-file default-directory "go.mod")
                               default-directory)))
    (compile "govulncheck .")))

;; Handy bindings in go-mode:
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-s p") #'jps-go-staticcheck-project)
  (define-key go-mode-map (kbd "C-c C-s .") #'jps-go-staticcheck-pkg)
  (define-key go-mode-map (kbd "C-c C-v p") #'jps-go-vulncheck-project)
  (define-key go-mode-map (kbd "C-c C-v .") #'jps-go-vulncheck-pkg))

;; --- Structlayout visualization ---
;; structlayout takes package path + type name as arguments
;; Usage: structlayout <package> <type>  (e.g., structlayout . User)

(defun jps--symbol-at-point-or-read (prompt)
  (let* ((sym (thing-at-point 'symbol t))
         (def (and sym (substring-no-properties sym))))
    (read-string prompt def)))

(defun jps--go-package-dir ()
  "Return the directory containing the current Go file."
  (when-let ((file (buffer-file-name)))
    (file-name-directory file)))

(defun jps-structlayout-pretty (type-name)
  "Display pretty ASCII layout for TYPE-NAME showing memory layout and padding."
  (interactive (list (jps--symbol-at-point-or-read "Struct type: ")))
  (let* ((pkg-dir (jps--go-package-dir))
         (default-directory (or pkg-dir default-directory))
         (tmp (generate-new-buffer " *structlayout-tmp*"))
         (out (get-buffer-create (format "*structlayout %s*" type-name))))
    (unwind-protect
        (progn
          (call-process "structlayout" nil tmp nil "." type-name)
          (with-current-buffer out (erase-buffer))
          (with-current-buffer tmp
            (call-process-region (point-min) (point-max) "structlayout-pretty" nil out))
          (pop-to-buffer out)
          (special-mode))
      (when (buffer-live-p tmp) (kill-buffer tmp)))))

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-l") #'jps-structlayout-pretty))

;; Transient menu for quick access (transient loaded in jps-core)
(transient-define-prefix jps-go-tools-menu ()
  "Go Tools"
  [["Staticcheck"
    ("s" "project ./..." jps-go-staticcheck-project)
    ("S" "package ."     jps-go-staticcheck-pkg)]
   ["Vulncheck"
    ("v" "project ./..." jps-go-vulncheck-project)
    ("V" "package ."     jps-go-vulncheck-pkg)]
   ["Struct"
    ("l" "layout"        jps-structlayout-pretty)]])

(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c C-t") #'jps-go-tools-menu))
;; Hit C-c C-t in a Go buffer for a quick pop-up of all the above.

(provide 'jps-lang-go)
;;; jps-lang-go.el ends here
