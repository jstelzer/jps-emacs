;;; jps-swift.el --- Swift language support -*- lexical-binding: t -*-
;;; Commentary:
;; Swift development environment with:
;; - swift-mode for editing
;; - sourcekit-lsp via Eglot
;; - optional editorconfig support
;; - swift-format and swiftlint helpers
;; - transient menu for common SwiftPM tasks
;;; Code:

(require 'use-package)
(require 'subr-x)
(require 'jps-core)

;;; ============================================================================
;;; Swift Mode
;;; ============================================================================

(use-package swift-mode
  :straight t
  :mode "\\.swift\\'"
  :interpreter "swift")

;; .editorconfig file support (useful for Swift style consistency)
(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

;;; ============================================================================
;;; LSP Configuration (sourcekit-lsp via Eglot)
;;; ============================================================================

(defun jps-swift--find-sourcekit-lsp ()
  "Locate the sourcekit-lsp executable, preferring system PATH."
  (or (executable-find "sourcekit-lsp")
      (and (eq system-type 'darwin)
           (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp")))
      "/usr/local/swift/usr/bin/sourcekit-lsp"))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(swift-mode . (,(jps-swift--find-sourcekit-lsp)))))

(add-hook 'swift-mode-hook #'eglot-ensure)

;;; ============================================================================
;;; SwiftPM Helpers
;;; ============================================================================

(defun jps-swift--project-root ()
  "Return the Swift project root (Package.swift), or fall back to jps project root."
  (or (locate-dominating-file default-directory "Package.swift")
      (jps--project-root)
      default-directory))

(defun jps-swift-build ()
  "Run 'swift build' from the Swift project root."
  (interactive)
  (let ((default-directory (jps-swift--project-root)))
    (compile "swift build")))

(defun jps-swift-test ()
  "Run 'swift test' from the Swift project root."
  (interactive)
  (let ((default-directory (jps-swift--project-root)))
    (compile "swift test")))

;;; ============================================================================
;;; Formatting & Linting (optional external tools)
;;; ============================================================================

(defun jps-swift-format-buffer ()
  "Format current file using swift-format (if installed)."
  (interactive)
  (unless (executable-find "swift-format")
    (user-error "swift-format not found in PATH"))
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (compile (format "swift-format format -i %s" (shell-quote-argument file)))))

(defun jps-swiftlint ()
  "Run swiftlint from the Swift project root (if installed)."
  (interactive)
  (unless (executable-find "swiftlint")
    (user-error "swiftlint not found in PATH"))
  (let ((default-directory (jps-swift--project-root)))
    (compile "swiftlint")))

(defun jps-swiftlint-autocorrect ()
  "Run swiftlint autocorrect from the Swift project root (if installed)."
  (interactive)
  (unless (executable-find "swiftlint")
    (user-error "swiftlint not found in PATH"))
  (let ((default-directory (jps-swift--project-root)))
    (compile "swiftlint autocorrect")))

;;; ============================================================================
;;; Transient Menu (Swift Tools)
;;; ============================================================================

(transient-define-prefix jps-swift-menu ()
  "Swift Tools"
  [["SwiftPM"
    ("b" "Build" jps-swift-build)
    ("t" "Test"  jps-swift-test)]
   ["Quality"
    ("f" "Format" jps-swift-format-buffer)
    ("l" "Lint"   jps-swiftlint)
    ("a" "Autocorrect" jps-swiftlint-autocorrect)]])

(with-eval-after-load 'swift-mode
  (define-key swift-mode-map (kbd "C-c C-t") #'jps-swift-menu))

(provide 'jps-swift)
;;; jps-swift.el ends here
