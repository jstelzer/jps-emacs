;;; jps-lang-python.el --- Python language support -*- lexical-binding: t -*-
;;; Commentary:
;; Python development environment with:
;; - pyenv for version management (auto-activates per project)
;; - blacken for code formatting
;; - pylsp + ruff for LSP
;; - Automatic virtualenv detection and activation
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

;; Python: env mgmt + formatter
(use-package blacken :straight t :hook (python-mode . blacken-mode))

;;; ============================================================================
;;; Python via pyenv (auto per project)
;;; ============================================================================

(use-package pyenv-mode
  :straight t
  :commands (pyenv-mode pyenv-mode-set pyenv-versions pyenv-which)
  :init
  (setenv "PYENV_ROOT" (or (getenv "PYENV_ROOT")
                           (expand-file-name "~/.pyenv")))
  (add-to-list 'exec-path (expand-file-name "bin" (getenv "PYENV_ROOT")))
  (pyenv-mode 1))

(defun jps--pyenv-version-for (dir)
  "Resolve the pyenv version name for DIR (reads .python-version or pyenv default)."
  (let* ((dot (jps--read-file-first-line (expand-file-name ".python-version" dir))))
    (if (and dot (not (string-empty-p dot)))
        dot
      (string-trim (with-output-to-string
                     (with-current-buffer standard-output
                       (call-process "pyenv" nil t nil "version-name")))))))

(defun jps--pyenv-virtualenv-dir (version)
  "Return directory of VERSION if it's a pyenv virtualenv; else nil."
  (let* ((root (getenv "PYENV_ROOT"))
         (vdir (and version (expand-file-name (concat "versions/" version) root))))
    (when (and vdir (file-exists-p (expand-file-name "bin/activate" vdir)))
      vdir)))

(defvar-local jps--active-pyenv-version nil)
(defvar-local jps--active-python-bin nil)

(defun jps--activate-pyenv-for-buffer ()
  "Activate pyenv version/venv for current Python buffer & configure tools/LSP."
  (when (derived-mode-p 'python-mode)
    (let* ((root (or (jps--project-root) default-directory))
           (ver  (jps--pyenv-version-for root))
           (venv-dir (jps--pyenv-virtualenv-dir ver))
           (python-bin (or (and ver (ignore-errors (pyenv-which "python")))
                           (executable-find "python")))
           (changed (not (equal ver jps--active-pyenv-version)))
           (version-source (if (file-readable-p (expand-file-name ".python-version" root))
                              (format ".python-version (%s)" (file-name-nondirectory (directory-file-name root)))
                            "pyenv global")))
      (when ver (pyenv-mode-set ver))
      ;; VIRTUAL_ENV for virtualenvs
      (if venv-dir
          (progn
            (setq-local process-environment
                        (cons (concat "VIRTUAL_ENV=" venv-dir)
                              (seq-remove (lambda (s) (string-prefix-p "VIRTUAL_ENV=" s))
                                          process-environment)))
            (add-to-list 'exec-path (expand-file-name "bin" venv-dir)))
        (setq-local process-environment
                    (seq-remove (lambda (s) (string-prefix-p "VIRTUAL_ENV=" s))
                                process-environment)))
      ;; Interpreter for REPL/tools
      (when python-bin
        (setq-local python-shell-interpreter python-bin)
        (setq jps--active-python-bin python-bin))
      (setq jps--active-pyenv-version ver)

      ;; Eglot server config (pylsp + ruff; pyright-based clients also hinted)
      (setq-local eglot-workspace-configuration
                  (let* ((base (or eglot-workspace-configuration '()))
                         (pyroot (expand-file-name "versions" (getenv "PYENV_ROOT")))
                         (venv-name (and venv-dir (file-name-nondirectory (directory-file-name venv-dir)))))
                    (append
                     `((python . ((venvPath . ,pyroot) (venv . ,venv-name)))
                       (pyright . ((venvPath . ,pyroot) (venv . ,venv-name)))
                       (basedpyright . ((venvPath . ,pyroot) (venv . ,venv-name)))
                       (pylsp . ((plugins . ((jedi . ((environment . ,(or venv-dir "")))))))))
                     base)))

      ;; Restart Eglot if env changed
      (when (and (bound-and-true-p eglot--managed-mode) changed)
        (ignore-errors (eglot-reconnect)))

      ;; Inform user of the active environment
      (when changed
        (message "Python: %s %s(from %s)"
                 ver
                 (if venv-dir "[virtualenv] " "")
                 version-source)))))

(add-hook 'python-mode-hook #'jps--activate-pyenv-for-buffer)
(add-hook 'find-file-hook (lambda () (when (eq major-mode 'python-mode)
                                       (jps--activate-pyenv-for-buffer))))

(defun jps-pyenv-switch (&optional version)
  "Interactively switch pyenv VERSION for this project/buffer and refresh Eglot."
  (interactive)
  (let* ((ver (or version (completing-read "pyenv version: " (pyenv-versions))))
         (root (or (jps--project-root) default-directory)))
    (with-temp-file (expand-file-name ".python-version" root)
      (insert ver "\n"))
    (message "Set .python-version => %s" ver)
    (jps--activate-pyenv-for-buffer)))

;; Helper to check if debugpy is available
(defun jps--python-has-debugpy-p ()
  "Check if debugpy is installed in the current Python environment."
  (and jps--active-python-bin
       (zerop (call-process jps--active-python-bin nil nil nil
                           "-c" "import debugpy"))))

;; Helper to install debugpy in current pyenv
(defun jps-python-install-debugpy ()
  "Install debugpy in the current pyenv environment."
  (interactive)
  (if jps--active-python-bin
      (let ((pip (expand-file-name "pip" (file-name-directory jps--active-python-bin))))
        (if (file-exists-p pip)
            (progn
              (message "Installing debugpy in %s..." jps--active-pyenv-version)
              (shell-command (format "%s install debugpy" pip))
              (message "debugpy installed successfully in %s!" jps--active-pyenv-version))
          (user-error "pip not found for current Python: %s" jps--active-python-bin)))
    (user-error "No active Python environment. Open a Python file first.")))

(defun jps-python-status ()
  "Show current Python environment status."
  (interactive)
  (if jps--active-pyenv-version
      (let* ((has-debugpy (jps--python-has-debugpy-p))
             (venv-dir (jps--pyenv-virtualenv-dir jps--active-pyenv-version))
             (root (or (jps--project-root) default-directory))
             (has-version-file (file-readable-p (expand-file-name ".python-version" root))))
        (message "Python: %s%s | debugpy: %s | source: %s | binary: %s"
                 jps--active-pyenv-version
                 (if venv-dir " [virtualenv]" "")
                 (if has-debugpy "installed" "NOT INSTALLED")
                 (if has-version-file ".python-version" "pyenv global")
                 (or jps--active-python-bin "unknown")))
    (message "No Python environment active. Open a Python file to activate.")))

(global-set-key (kbd "C-c p y") #'jps-pyenv-switch)
(global-set-key (kbd "C-c p i") #'jps-python-status)

;; Keep Black/Ruff/etc on the selected interpreter
(with-eval-after-load 'blacken
  (add-hook 'python-mode-hook
            (lambda ()
              (when jps--active-python-bin
                (setq-local blacken-executable
                            (or (executable-find "black")
                                (expand-file-name "black" (concat (file-name-directory jps--active-python-bin) "../bin/"))))))))

;; LSP server choice for Python: pylsp + ruff (single LSP lane)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (setq-default eglot-workspace-configuration
                (append
                 '((pylsp . ((plugins . ((pyflakes . (:enabled nil))
                                         (mccabe . (:enabled nil))
                                         (pycodestyle . (:enabled nil))
                                         (ruff . (:enabled t))
                                         (black . (:enabled nil))
                                         (yapf . (:enabled nil))
                                         (pydocstyle . (:enabled nil))))))
                   (python . ((analysis . ((diagnosticMode . "openFilesOnly"))))))
                 eglot-workspace-configuration)))

(provide 'jps-lang-python)
;;; jps-lang-python.el ends here
