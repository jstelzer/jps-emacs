;;; jps-debug.el --- Debug Adapter Protocol (DAP) for multiple languages -*- lexical-binding: t -*-
;;; Commentary:
;; DAP (Debug Adapter Protocol) integration for:
;; - Go (via dlv-go)
;; - Rust (via CodeLLDB)
;; - Python (via debugpy)
;;
;; Provides consistent debugging interface across languages
;;; Code:

(require 'use-package)

;;; ============================================================================
;;; DAP (Debug Adapter Protocol) for Go, Rust, Python
;;; ============================================================================

(use-package dap-mode
  :straight t
  :after eglot
  :config
  (require 'dap-dlv-go)
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  ;; Enable mouse hover support (optional but helpful)
  (dap-ui-controls-mode 1))

;;; ============================================================================
;;; Go Debugging (dlv-go)
;;; ============================================================================

;; Key bindings for debugging (consistent with your C-c style)
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c d b") #'dap-breakpoint-toggle)
  (define-key go-mode-map (kbd "C-c d d") #'dap-debug)
  (define-key go-mode-map (kbd "C-c d n") #'dap-next)
  (define-key go-mode-map (kbd "C-c d i") #'dap-step-in)
  (define-key go-mode-map (kbd "C-c d o") #'dap-step-out)
  (define-key go-mode-map (kbd "C-c d c") #'dap-continue)
  (define-key go-mode-map (kbd "C-c d r") #'dap-restart-frame)
  (define-key go-mode-map (kbd "C-c d q") #'dap-disconnect)
  (define-key go-mode-map (kbd "C-c d l") #'dap-ui-locals)
  (define-key go-mode-map (kbd "C-c d s") #'dap-ui-sessions))

;; Debug template for PAM test-server (adjust path as needed)
(with-eval-after-load 'dap-dlv-go
  (dap-register-debug-template "PAM Test Server"
    (list :type "go"
          :request "launch"
          :name "Start Test Server (Debug)"
          :mode "debug"
          :buildFlags ["-gcflags=all=-N -l"]
          :program (expand-file-name "~/projects/llc.neverlight/pam/cmd/local/start-test-server/main.go")
          :cwd (expand-file-name "~/projects/llc.neverlight/pam")
          :args nil)))

;;; ============================================================================
;;; DAP UI Faces
;;; ============================================================================

(with-eval-after-load 'dap-ui
  ;; Make the paused line pop (works well on manoj-dark)
  (when (facep 'dap-ui-marker-face)
    (set-face-attribute 'dap-ui-marker-face nil
                        :background "goldenrod" :foreground "black"
                        :weight 'bold :inherit 'default :extend t))

  ;; Breakpoints: verified vs pending
  (when (facep 'dap-ui-verified-breakpoint-face)
    (set-face-attribute 'dap-ui-verified-breakpoint-face nil
                        :background "firebrick" :foreground "white" :weight 'bold))
  (when (facep 'dap-ui-pending-breakpoint-face)
    (set-face-attribute 'dap-ui-pending-breakpoint-face nil
                        :background "DarkRed" :foreground "white")))

(require 'dap-hydra)
(with-eval-after-load 'go-mode
  (define-key go-mode-map (kbd "C-c d h") #'dap-hydra))
(setq dap-ui-marker-glyph "▶")

;;; ============================================================================
;;; Rust Debugging (CodeLLDB)
;;; ============================================================================

;; Rust debugging with CodeLLDB (install: https://github.com/vadimcn/codelldb)
;; Installation methods:
;;   Linux (Arch): yay -S codelldb-bin
;;   macOS: Download from https://github.com/vadimcn/codelldb/releases
;;          Extract to ~/codelldb or /usr/local/opt/codelldb
;;   OR: Let dap-mode auto-install via M-x dap-codelldb-setup

(with-eval-after-load 'dap-mode
  (require 'dap-codelldb)
  ;; Point to system codelldb installation (cross-platform)
  (setq dap-codelldb-debug-program
        (cond
         ;; Linux system package
         ((file-exists-p "/usr/lib/codelldb/adapter/codelldb")
          "/usr/lib/codelldb/adapter/codelldb")
         ;; macOS manual install (user directory)
         ((file-exists-p (expand-file-name "~/codelldb/adapter/codelldb"))
          (expand-file-name "~/codelldb/adapter/codelldb"))
         ;; macOS manual install (system directory)
         ((file-exists-p "/usr/local/opt/codelldb/adapter/codelldb")
          "/usr/local/opt/codelldb/adapter/codelldb")
         ;; Check if codelldb is in PATH
         ((executable-find "codelldb")
          (executable-find "codelldb"))
         ;; dap-mode auto-installed VSCode extension
         ((file-exists-p (expand-file-name "~/.emacs.d/.extension/vscode/codelldb/extension/adapter/codelldb"))
          (expand-file-name "~/.emacs.d/.extension/vscode/codelldb/extension/adapter/codelldb"))
         ;; Fall back to dap-mode's default (will prompt to install if missing)
         (t (list "node"
                  (expand-file-name "~/.emacs.d/.extension/vscode/codelldb/extension/adapter/main.js"))))))

(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c d b") #'dap-breakpoint-toggle)
  (define-key rust-mode-map (kbd "C-c d d") #'dap-debug)
  (define-key rust-mode-map (kbd "C-c d n") #'dap-next)
  (define-key rust-mode-map (kbd "C-c d i") #'dap-step-in)
  (define-key rust-mode-map (kbd "C-c d o") #'dap-step-out)
  (define-key rust-mode-map (kbd "C-c d c") #'dap-continue)
  (define-key rust-mode-map (kbd "C-c d r") #'dap-restart-frame)
  (define-key rust-mode-map (kbd "C-c d q") #'dap-disconnect)
  (define-key rust-mode-map (kbd "C-c d l") #'dap-ui-locals)
  (define-key rust-mode-map (kbd "C-c d s") #'dap-ui-sessions)
  (define-key rust-mode-map (kbd "C-c d h") #'dap-hydra))

;; Debug template for Rust projects
(with-eval-after-load 'dap-codelldb
  ;; Simple template that prompts for the binary to debug
  (dap-register-debug-template "Rust::Run"
    (list :type "lldb"
          :request "launch"
          :name "Rust::Run"
          :program nil  ;; Will prompt for program path
          :args []
          :cwd nil))    ;; Will use current directory

  ;; Cargo-based template (builds the project first)
  (dap-register-debug-template "Rust::Cargo Run"
    (list :type "lldb"
          :request "launch"
          :name "Rust::Cargo Run"
          :cargo (list :args ["build" "--bin=${command:pickArgs}"]
                       :filter (list :name "${command:pickArgs}" :kind "bin"))
          :args []
          :cwd "${workspaceFolder}"))

  (dap-register-debug-template "Rust::Test"
    (list :type "lldb"
          :request "launch"
          :name "Rust::Test"
          :cargo (list :args ["test" "--no-run"]
                       :filter (list :name "${command:pickArgs}" :kind "test"))
          :args []
          :cwd "${workspaceFolder}")))

;;; ============================================================================
;;; Python Debugging (debugpy)
;;; ============================================================================

;; Python debugging with debugpy (install: pip install debugpy)
;; Integrates with your pyenv setup automatically
(with-eval-after-load 'dap-mode
  (require 'dap-python))

;; Key bindings for Python debugging (set in hook to ensure proper loading)
(defun jps--setup-python-debug-keys ()
  "Set up Python debugging keybindings."
  (when (fboundp 'jps-python-debug-file)
    (define-key python-mode-map (kbd "C-c d b") #'dap-breakpoint-toggle)
    (define-key python-mode-map (kbd "C-c d d") #'jps-python-debug-file)
    (define-key python-mode-map (kbd "C-c d D") #'dap-debug)  ;; fallback to template selector
    (define-key python-mode-map (kbd "C-c d n") #'dap-next)
    (define-key python-mode-map (kbd "C-c d i") #'dap-step-in)
    (define-key python-mode-map (kbd "C-c d o") #'dap-step-out)
    (define-key python-mode-map (kbd "C-c d c") #'dap-continue)
    (define-key python-mode-map (kbd "C-c d r") #'dap-restart-frame)
    (define-key python-mode-map (kbd "C-c d q") #'dap-disconnect)
    (define-key python-mode-map (kbd "C-c d l") #'dap-ui-locals)
    (define-key python-mode-map (kbd "C-c d s") #'dap-ui-sessions)
    (define-key python-mode-map (kbd "C-c d h") #'dap-hydra)))

(add-hook 'python-mode-hook #'jps--setup-python-debug-keys)

;; Enable dap-mode in Python buffers
(add-hook 'python-mode-hook #'dap-mode)

;; Auto-install debugpy when debugging is first attempted
(defun jps--ensure-debugpy-installed ()
  "Ensure debugpy is installed, auto-installing if missing."
  (when (and (derived-mode-p 'python-mode)
             (not (jps--python-has-debugpy-p)))
    (if (y-or-n-p (format "debugpy not found in %s. Install it? " jps--active-pyenv-version))
        (jps-python-install-debugpy)
      (user-error "debugpy is required for Python debugging. Install with: M-x jps-python-install-debugpy"))))

;; Hook into dap-debug to ensure debugpy is available
(advice-add 'dap-debug :before
            (lambda (&rest _)
              (when (derived-mode-p 'python-mode)
                (jps--ensure-debugpy-installed))))

;; Configure dap-python to use the active pyenv interpreter
(with-eval-after-load 'dap-python
  ;; Point to active pyenv python (updated by jps--activate-pyenv-for-buffer)
  (setq dap-python-debugger 'debugpy)

  ;; Set a default executable (will be overridden per-buffer by jps--dap-python-setup)
  (setq dap-python-executable (or (executable-find "python3")
                                   (executable-find "python")
                                   "python"))

  ;; Auto-configure debugpy path from active pyenv
  (defun jps--dap-python-setup ()
    "Configure dap-python to use the current pyenv Python."
    (when (and (derived-mode-p 'python-mode) jps--active-python-bin)
      (setq-local dap-python-executable jps--active-python-bin)))

  (add-hook 'python-mode-hook #'jps--dap-python-setup)

  ;; Ensure executable is set before any debug operation
  (advice-add 'dap-debug :before
              (lambda (&rest _)
                (when (derived-mode-p 'python-mode)
                  ;; Always ensure dap-python-executable is set (globally)
                  (let ((python-exe (or jps--active-python-bin
                                       (bound-and-true-p dap-python-executable)
                                       (executable-find "python3")
                                       (executable-find "python"))))
                    (unless python-exe
                      (user-error "Cannot find Python executable. Is pyenv configured?"))
                    (setq dap-python-executable python-exe))))))

  ;; Custom function to debug current Python file with proper configuration
  (defun jps-python-debug-file ()
    "Debug the current Python file with a properly configured debug session."
    (interactive)
    (unless (derived-mode-p 'python-mode)
      (user-error "Not in a Python buffer"))
    (let* ((file (buffer-file-name))
           (dir (file-name-directory file))
           (python-exe (or jps--active-python-bin
                          dap-python-executable
                          (executable-find "python3")
                          (executable-find "python")
                          "python")))
      ;; Validate that we have all required values
      (unless file
        (user-error "Buffer has no associated file"))
      (unless dir
        (user-error "Cannot determine working directory"))
      (unless python-exe
        (user-error "Cannot find Python executable"))
      ;; Ensure dap-python-executable is set globally before creating session
      (setq dap-python-executable python-exe)
      (let ((debug-config
             (list :type "python"
                   :request "launch"
                   :name "Debug Python File"
                   :program file
                   :cwd dir
                   :args ""
                   :debugger 'debugpy
                   :python python-exe)))
        (dap-debug debug-config))))

  ;; Debug template for Python scripts (with proper defaults)
  (dap-register-debug-template "Python :: Run file"
    (list :type "python"
          :args ""
          :cwd "${workspaceFolder}"
          :module nil
          :program "${file}"
          :request "launch"
          :debugger 'debugpy
          :name "Python :: Run file"))

  (dap-register-debug-template "Python :: Run module"
    (list :type "python"
          :args ""
          :cwd "${workspaceFolder}"
          :module "${command:pickArgs}"
          :program nil
          :request "launch"
          :debugger 'debugpy
          :name "Python :: Run module"))

  (dap-register-debug-template "Python :: Pytest current file"
    (list :type "python"
          :args (list "-s" "${file}")
          :cwd "${workspaceFolder}"
          :program nil
          :module "pytest"
          :request "launch"
          :debugger 'debugpy
          :name "Python :: Pytest current file"))

(provide 'jps-debug)
;;; jps-debug.el ends here
