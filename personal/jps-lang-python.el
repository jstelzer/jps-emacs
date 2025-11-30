;;; jps-lang-python.el --- Python language support -*- lexical-binding: t -*-
;;; Commentary:
;; Python development environment with:
;; - uv for Python version & venv management
;; - basedpyright for LSP (types, completions, navigation)
;; - ruff for linting + formatting
;; - Automatic .venv detection and activation
;;; Code:

(require 'use-package)
(require 'jps-core)

;;; ============================================================================
;;; Python Language Support
;;; ============================================================================

;; Ensure built-in python mode is loaded
(use-package python
  :straight nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;;; ============================================================================
;;; Python via uv (auto per project)
;;; ============================================================================

;; uv workflow:
;;   uv python install 3.12       # install Python version
;;   uv python list               # list installed versions
;;   uv python pin 3.12           # create .python-version
;;   uv venv                      # create .venv in project
;;   uv sync                      # install deps from pyproject.toml
;;   uv pip install <pkg>         # install into .venv

(defvar-local jps--active-venv-dir nil
  "Path to the active .venv directory for current buffer.")

(defvar-local jps--active-python-bin nil
  "Path to the active Python binary for current buffer.")

(defvar-local jps--active-python-version nil
  "Python version string for current buffer (e.g. \"3.12.2\").")

(defun jps--uv-find-venv (dir)
  "Find .venv directory starting from DIR, searching upward."
  (let ((venv (locate-dominating-file dir ".venv")))
    (when venv
      (let ((venv-path (expand-file-name ".venv" venv)))
        (when (file-directory-p venv-path)
          venv-path)))))

(defun jps--uv-python-version (python-bin)
  "Get Python version string from PYTHON-BIN."
  (when (and python-bin (file-executable-p python-bin))
    (string-trim
     (shell-command-to-string
      (format "%s -c \"import sys; print('.'.join(map(str, sys.version_info[:3])))\""
              (shell-quote-argument python-bin))))))

(defun jps--activate-uv-venv-for-buffer ()
  "Activate uv venv for current Python buffer & configure tools/LSP."
  (when (derived-mode-p 'python-mode)
    (let* ((root (or (jps--project-root) default-directory))
           (venv-dir (jps--uv-find-venv root))
           (python-bin (if venv-dir
                           (expand-file-name "bin/python" venv-dir)
                         (or (executable-find "python3")
                             (executable-find "python"))))
           (changed (not (equal venv-dir jps--active-venv-dir))))

      ;; Set VIRTUAL_ENV for tools that check it
      (if venv-dir
          (progn
            (setq-local process-environment
                        (cons (concat "VIRTUAL_ENV=" venv-dir)
                              (seq-remove (lambda (s) (string-prefix-p "VIRTUAL_ENV=" s))
                                          process-environment)))
            ;; Add venv bin to exec-path
            (setq-local exec-path (cons (expand-file-name "bin" venv-dir) exec-path)))
        ;; No venv - clear VIRTUAL_ENV
        (setq-local process-environment
                    (seq-remove (lambda (s) (string-prefix-p "VIRTUAL_ENV=" s))
                                process-environment)))

      ;; Set Python interpreter
      (when python-bin
        (setq-local python-shell-interpreter python-bin)
        (setq jps--active-python-bin python-bin)
        (setq jps--active-python-version (jps--uv-python-version python-bin)))

      (setq jps--active-venv-dir venv-dir)

      ;; Eglot server config (basedpyright venv awareness)
      (when venv-dir
        (setq-local eglot-workspace-configuration
                    (let ((base (or eglot-workspace-configuration '()))
                          (venv-parent (file-name-directory (directory-file-name venv-dir))))
                      (append
                       `((python . ((venvPath . ,venv-parent) (venv . ".venv")))
                         (basedpyright . ((venvPath . ,venv-parent) (venv . ".venv"))))
                       base))))

      ;; Restart Eglot if venv changed
      (when (and (bound-and-true-p eglot--managed-mode) changed)
        (ignore-errors (eglot-reconnect)))

      ;; Inform user of the active environment
      (when changed
        (if venv-dir
            (message "Python: %s [.venv] @ %s"
                     (or jps--active-python-version "unknown")
                     (abbreviate-file-name venv-dir))
          (message "Python: %s (system)" (or jps--active-python-version "unknown")))))))

(add-hook 'python-mode-hook #'jps--activate-uv-venv-for-buffer)
(add-hook 'find-file-hook (lambda () (when (eq major-mode 'python-mode)
                                       (jps--activate-uv-venv-for-buffer))))

;;; ============================================================================
;;; uv Project Commands
;;; ============================================================================

(defun jps-uv-venv-create ()
  "Create a .venv in the current project using uv."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (compile "uv venv")
    (run-at-time 2 nil #'jps--activate-uv-venv-for-buffer)))

(defun jps-uv-sync ()
  "Sync dependencies from pyproject.toml using uv."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (compile "uv sync")))

(defun jps-uv-add (package)
  "Add PACKAGE to project dependencies using uv."
  (interactive "sPackage to add: ")
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (compile (format "uv add %s" (shell-quote-argument package)))))

(defun jps-uv-python-pin (version)
  "Pin Python VERSION for current project (creates .python-version)."
  (interactive
   (list (completing-read "Python version: "
                          (split-string
                           (shell-command-to-string "uv python list --only-installed | awk '{print $1}'")
                           "\n" t))))
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (shell-command (format "uv python pin %s" (shell-quote-argument version)))
    (message "Pinned Python %s" version)))

;; Helper to check if debugpy is available
(defun jps--python-has-debugpy-p ()
  "Check if debugpy is installed in the current Python environment."
  (and jps--active-python-bin
       (zerop (call-process jps--active-python-bin nil nil nil
                           "-c" "import debugpy"))))

;; Install all dev tools in current venv
(defvar jps-python-dev-tools
  '("debugpy" "ruff" "basedpyright" "pip-audit" "mypy" "pytest")
  "Python development tools to install via `jps-python-install-dev-tools'.")

(defun jps-python-install-dev-tools ()
  "Install all dev tools in the current venv using uv."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory))
         (tools (string-join jps-python-dev-tools " ")))
    (if jps--active-venv-dir
        (compile (format "uv pip install %s" tools))
      (user-error "No .venv found. Run `jps-uv-venv-create' first"))))

(defun jps-python-status ()
  "Show current Python environment status."
  (interactive)
  (let* ((has-debugpy (jps--python-has-debugpy-p))
         (root (or (jps--project-root) default-directory))
         (has-pyproject (file-exists-p (expand-file-name "pyproject.toml" root))))
    (message "Python: %s | venv: %s | debugpy: %s | pyproject.toml: %s | binary: %s"
             (or jps--active-python-version "unknown")
             (if jps--active-venv-dir
                 (abbreviate-file-name jps--active-venv-dir)
               "none")
             (if has-debugpy "yes" "NO")
             (if has-pyproject "yes" "no")
             (or jps--active-python-bin "unknown"))))

(global-set-key (kbd "C-c p s") #'jps-python-status)

;;; ============================================================================
;;; Advanced Tooling
;;; ============================================================================

;; External tools (install via jps-python-install-dev-tools or uv pip install):
;;   ruff           # linting + formatting
;;   pip-audit      # vulnerability scanning
;;   mypy           # type checking
;;   pytest         # testing
;;   basedpyright   # LSP server

(defun jps-python-audit ()
  "Run pip-audit to check dependencies for known vulnerabilities."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (compile "uv run pip-audit")))

(defun jps-python-typecheck ()
  "Run mypy on the current project."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (compile "uv run mypy .")))

(defun jps-python-lint ()
  "Run ruff check on the current project."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (compile "uv run ruff check .")))

(defun jps-python-format ()
  "Run ruff format on the current buffer's file."
  (interactive)
  (when buffer-file-name
    (let ((default-directory (or (jps--project-root) default-directory)))
      (shell-command (format "uv run ruff format %s" (shell-quote-argument buffer-file-name)))
      (revert-buffer t t t))))

(defun jps-python-test ()
  "Run pytest on the current project."
  (interactive)
  (let* ((root (jps--project-root))
         (default-directory (or root default-directory)))
    (compile "uv run pytest")))

(defun jps-python-test-file ()
  "Run pytest on the current file."
  (interactive)
  (when buffer-file-name
    (let ((default-directory (or (jps--project-root) default-directory)))
      (compile (format "uv run pytest %s -v" (shell-quote-argument buffer-file-name))))))

;; Format and organize imports on save via ruff
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (and buffer-file-name jps--active-venv-dir)
                          (let ((default-directory (or (jps--project-root) default-directory)))
                            (shell-command (format "uv run ruff check --select I --fix %s 2>/dev/null"
                                                   (shell-quote-argument buffer-file-name)))
                            (shell-command (format "uv run ruff format %s 2>/dev/null"
                                                   (shell-quote-argument buffer-file-name))))))
                      nil t)
            (add-hook 'after-save-hook
                      (lambda () (revert-buffer t t t))
                      nil t)))

;; LSP server: basedpyright (types/completions/navigation)
;; Install: uv pip install basedpyright
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-mode . ("basedpyright-langserver" "--stdio")))
  (setq-default eglot-workspace-configuration
                (append
                 '((basedpyright . ((analysis . ((diagnosticMode . "openFilesOnly")
                                                  (typeCheckingMode . "standard")
                                                  (autoImportCompletions . t)
                                                  (inlayHints . ((variableTypes . t)
                                                                 (functionReturnTypes . t)
                                                                 (callArgumentNames . t)
                                                                 (pytestParameters . t))))))))
                 eglot-workspace-configuration)))

;;; ============================================================================
;;; Transient Menu
;;; ============================================================================

;; transient loaded in jps-core
(transient-define-prefix jps-python-menu ()
  "Python Commands"
  [["Check"
    ("c" "Lint (ruff)"    jps-python-lint)
    ("m" "Typecheck (mypy)" jps-python-typecheck)
    ("a" "Audit (vulns)"  jps-python-audit)]
   ["Test"
    ("t" "Test (project)" jps-python-test)
    ("T" "Test (file)"    jps-python-test-file)]
   ["Format"
    ("f" "Format (ruff)"  jps-python-format)]
   ["Environment"
    ("s" "Status"         jps-python-status)
    ("v" "Create .venv"   jps-uv-venv-create)
    ("S" "Sync deps"      jps-uv-sync)
    ("p" "Pin Python"     jps-uv-python-pin)
    ("i" "Install tools"  jps-python-install-dev-tools)]])

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-t") #'jps-python-menu))

(provide 'jps-lang-python)
;;; jps-lang-python.el ends here
