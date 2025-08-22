;;; jps.el --- Personal Emacs configuration
;;; Commentary:
;; Personal customizations and functions for Emacs
;;; Code:

;;; ============================================================================
;;; Basic Configuration
;;; ============================================================================

;; PATH synchronization (essential for external tools)
(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

;; Basic UI settings
(when (display-graphic-p)
  (tool-bar-mode -1)
  (set-face-attribute 'default nil :family "MesloLGS NF" :height 120))

;; Start server only in interactive mode
(unless noninteractive (server-start))

;; Personal information
(setq add-log-mailing-address "mental@neverlight.com"
      add-log-full-name "Jason Stelzer")
;; --- Eglot workspace config holder (prevents void-variable errors) -----------
(defvar eglot-workspace-configuration nil
  "Per-language configuration map passed to LSP servers via Eglot.")

;; Python shell defaults (kept for convenience; pyenv will override interpreter)
(setq py-python-command "ipython"
      py-python-command-args '("--pylab" "inline" "--colors" "NoColor"))

;; File handling
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(global-font-lock-mode t)

;; Interface improvements
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq c-default-style "user"
      c-basic-offset 4)

;;; ============================================================================
;;; Custom Functions
;;; ============================================================================

(defun jps-kill-line ()
  "Emulate vi's 'dd' command."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (delete-blank-lines))

(defun jps-diff-buffer ()
  "Show the difference between the current buffer and the file on disk."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(defun jps-add-path (path-element)
  "Add PATH-ELEMENT to PATH and exec-path."
  (interactive "DAdd to PATH: ")
  (when (and (file-directory-p path-element)
             (not (string-match (regexp-quote (expand-file-name path-element))
                                (getenv "PATH"))))
    (setenv "PATH" (concat (expand-file-name path-element)
                           path-separator (getenv "PATH")))
    (setq exec-path (parse-colon-path (getenv "PATH")))))

(defun jps-json-format (b e)
  "Format region B..E as JSON using python json.tool."
  (interactive "r")
  (shell-command-on-region b e "python -m json.tool" (current-buffer) t))

(defun jps-json-flatten (b e)
  "Flatten region B..E into a single-line, valid JSON."
  (interactive "r")
  (shell-command-on-region
   b e "python -c 'import sys,json; print(json.dumps(json.load(sys.stdin), separators=(\",\", \":\")))'"
   (current-buffer) t))

(defun jps-generate-timestamp ()
  "Insert a timestamp at point."
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

(defun jps-toggle-transparency ()
  "Toggle frame transparency between opaque and semi-transparent on any build."
  (interactive)
  (let* ((frame (selected-frame))
         (ab (frame-parameter frame 'alpha-background))
         (have-ab (numberp ab))
         (opaque (if have-ab 1.0 100))
         (trans  (if have-ab 0.75 75)))
    (if have-ab
        (set-frame-parameter frame 'alpha-background
                             (if (< ab 1.0) opaque trans))
      (let* ((cur (frame-parameter frame 'alpha))
             (cur-active (cond ((numberp cur) cur)
                               ((consp cur) (car cur))
                               (t 100))))
        (set-frame-parameter frame 'alpha
                             (if (< cur-active 100)
                                 (cons opaque opaque)
                               (cons trans trans)))))))

(defun jps-configure-platform ()
  "Configure platform-specific options."
  (interactive)
  (when (eq system-type 'darwin)
    (message "Configuring macOS...")
    (when (display-graphic-p)
      (setq default-input-method "MacOSX")
      (dolist (event '([wheel-down] [double-wheel-down] [triple-wheel-down]))
        (global-set-key event (lambda () (interactive) (scroll-down 1))))
      (dolist (event '([wheel-up] [double-wheel-up] [triple-wheel-up]))
        (global-set-key event (lambda () (interactive) (scroll-up 1)))))
    (jps-add-path "/usr/local/bin")))

;;; ============================================================================
;;; Platform Configuration
;;; ============================================================================

(jps-configure-platform)

;; Add home bin directories to PATH
(dolist (path (list (concat (getenv "HOME") "/bin")
                    (concat (getenv "HOME") "/.cargo/bin")))
  (when (file-directory-p path)
    (jps-add-path path)))

;;; ============================================================================
;;; Development Environment
;;; ============================================================================

;; LSP via Eglot
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

;; Docker/YAML modes
(use-package dockerfile-mode :straight t)
(use-package yaml-mode       :straight t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\`Dockerfile\\'" . dockerfile-mode))

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

;; Rust
(use-package rust-mode :straight t)
(use-package cargo     :straight t :hook (rust-mode . cargo-minor-mode))
(use-package rust-playground :straight t)
(with-eval-after-load 'rust-mode
  (setq rust-format-on-save t))
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

;; Shared helper (also used by dashboard funcs later)
(require 'project)
(defvar jps--last-compile-cmd nil)

(defun jps--project-root ()
  (when-let* ((proj (project-current t)))
    (expand-file-name (project-root proj))))

(defun jps--read-file-first-line (f)
  (when (file-readable-p f)
    (with-temp-buffer
      (insert-file-contents f)
      (string-trim (buffer-substring-no-properties (point-min) (line-end-position))))))

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
           (changed (not (equal ver jps--active-pyenv-version))))
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
        (ignore-errors (eglot-reconnect))))))

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
(global-set-key (kbd "C-c p y") #'jps-pyenv-switch)

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

;;; ============================================================================
;;; Code quality & completion
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

;;; ============================================================================
;;; Modern Emacs UI
;;; ============================================================================

(use-package vertico   :straight t :init (vertico-mode 1))
(use-package orderless :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))
(use-package marginalia :straight t :init (marginalia-mode 1))

(use-package consult
  :straight t
  :bind (("C-S-p" . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :init
  (setq consult-find-command
        "fd --hidden --follow --exclude .git --color=never -t f \\S- || find . -type f"))

(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult :straight t :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package project :straight nil
  :init (setq project-vc-extra-root-markers '(".git"))
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Grep")
          (magit-project-status "Magit")
          (jps-project-vterm "Shell")
          (jps-project-test "Test")
          (jps-project-build "Build")
          (jps-project-deploy "Deploy")
          (jps-project-open-compose "Compose")
          (jps-project-notes "Notes"))))

;; Treemacs
(use-package treemacs :straight t
  :bind (("C-x t t" . treemacs)
         ("C-x t 1" . treemacs-delete-other-windows)
         ("C-x t b" . treemacs-bookmark))
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-git-mode 'deferred)
  (when (fboundp 'treemacs-hide-gitignored-files-mode)
    (treemacs-hide-gitignored-files-mode 1)))
(use-package treemacs-project-follow-mode :straight nil
  :after treemacs
  :config (treemacs-project-follow-mode 1))

(use-package which-key :straight t
  :init (which-key-mode 1)
  :custom (which-key-idle-delay 0.4))

;;; ============================================================================
;;; External Tools Integration
;;; ============================================================================

(use-package vterm :straight t)

(use-package magit :straight t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter :straight t
  :hook (prog-mode . git-gutter-mode)
  :config (setq git-gutter:update-interval 0.02))

(use-package deadgrep :straight t
  :bind ("C-x p G" . deadgrep))

(use-package restclient :straight t
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (defun jps-restclient-extract-token ()
    "Extract token from last response and insert/update :token variable."
    (interactive)
    (let ((token nil))
      (save-excursion
        (when (get-buffer "*HTTP Response*")
          (with-current-buffer "*HTTP Response*"
            (goto-char (point-min))
            (when (re-search-forward "\"id_token\"\\s-*:\\s-*\"\\([^\"]+\\)\"" nil t)
              (setq token (match-string 1))))))
      (when token
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^:token = .*$" nil t)
              (replace-match (format ":token = %s" token))
            (goto-char (point-min))
            (insert (format ":token = %s\n\n" token))))
        (message "Token extracted and set!"))
      (unless token (message "No id_token found in response"))))
  :bind (:map restclient-mode-map
              ("C-c C-t" . jps-restclient-extract-token)))

(use-package docker :straight t :bind ("C-c D" . docker))

(use-package multiple-cursors :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

(use-package markdown-mode :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config (claude-code-ide-emacs-tools-setup))

;; REST ergonomics
(use-package company-restclient :straight t)
(use-package jq-mode           :straight t) ; view JSON with jq
(use-package graphql-mode      :straight t)
(use-package ob-restclient     :straight t
  :after (org restclient)
  :init (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))

(with-eval-after-load 'restclient
  (add-hook 'restclient-mode-hook
            (lambda ()
              (setq-local company-backends '((company-restclient company-dabbrev-code)))
              (company-mode 1)
              (visual-line-mode 1))))

(defun jps-project-open-api ()
  "Open or create the project's api.http scratchpad."
  (interactive)
  (let* ((root (jps--project-root))
         (file (expand-file-name "api.http" root)))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "# Project API scratchpad\n"
                "# Tip: C-c C-c sends the request under point\n\n"
                ":env = dev\n"
                ":base_url = http://localhost:8080\n"
                ":token = \n\n"
                "### Health\nGET :base_url/health\n\n"
                "### Login\nPOST :base_url/login\nContent-Type: application/json\n\n"
                "{\n  \"username\": \"user\",\n  \"password\": \"pass\"\n}\n\n"
                "### Authorized example\nGET :base_url/me\nAuthorization: Bearer :token\n")))
    (find-file file)))
(define-key project-prefix-map (kbd "A") #'jps-project-open-api)
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements project-prefix-map "A" "API scratchpad"))

(defun jps-restclient-set-bearer-from-response ()
  "Grab id_token/access_token from *HTTP Response* and set :token in current .http buffer."
  (interactive)
  (let ((tok nil))
    (when (get-buffer "*HTTP Response*")
      (with-current-buffer "*HTTP Response*"
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "\"\\(id_token\\|access_token\\)\"\\s-*:\\s-*\"\\([^\"]+\\)\"" nil t)
            (setq tok (match-string 2))))))
    (if tok
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^:token\\s-*=" nil t)
              (replace-match (format ":token = %s" tok) t t)
            (goto-char (point-min))
            (insert (format ":token = %s\n" tok) "\n"))
          (message "Bearer :token updated."))
      (message "No id_token/access_token in *HTTP Response*."))))
(with-eval-after-load 'restclient
  (define-key restclient-mode-map (kbd "C-c C-b") #'jps-restclient-set-bearer-from-response))

(defun jps-restclient-set-env (name)
  "Set :env variable at top of current .http buffer to NAME."
  (interactive (list (completing-read "env: " '("dev" "stage" "prod") nil t)))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^:env\\s-*=" nil t)
        (replace-match (format ":env = %s" name) t t)
      (goto-char (point-min))
      (insert (format ":env = %s\n\n" name))))
  (message "env => %s" name))
(with-eval-after-load 'restclient
  (define-key restclient-mode-map (kbd "C-c C-e") #'jps-restclient-set-env))

(defun jps-restclient-jq (filter)
  "Run jq FILTER over the *HTTP Response* JSON and show in a temp buffer."
  (interactive "sjq filter: ")
  (let ((resp (get-buffer "*HTTP Response*")))
    (unless resp (user-error "No *HTTP Response* buffer"))
    (with-current-buffer resp
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward "^{\\|\\[" nil t)
          (user-error "No JSON payload found in *HTTP Response*")))
      (let* ((json (buffer-substring-no-properties (match-beginning 0) (point-max)))
             (buf (get-buffer-create "*HTTP Response | jq*")))
        (with-current-buffer buf
          (erase-buffer)
          (let* ((tmp (make-temp-file "resp" nil ".json" json))
                 (cmd (format "jq '%s' %s" filter (shell-quote-argument tmp))))
            (call-process-shell-command cmd nil t t)
            (delete-file tmp))
          (jq-mode))
        (pop-to-buffer buf)))))
(with-eval-after-load 'restclient
  (define-key restclient-mode-map (kbd "C-c C-j") #'jps-restclient-jq))

;;; ============================================================================
;;; Project Dashboard (C-x p …)
;;; ============================================================================

(defun jps--run-in-project (cmd)
  "Run shell CMD from the project root using `compile'."
  (interactive "sRun in project: ")
  (let ((default-directory (jps--project-root)))
    (setq jps--last-compile-cmd cmd)
    (compile cmd)))

(defun jps-project-ripgrep ()
  "Ripgrep the project using consult-ripgrep."
  (interactive)
  (let ((default-directory (jps--project-root)))
    (consult-ripgrep default-directory)))

(defun jps-project-vterm ()
  "Open a vterm in the project root."
  (interactive)
  (let* ((root (jps--project-root))
         (name (format "*vterm: %s*" (file-name-nondirectory (directory-file-name root)))))
    (let ((default-directory root))
      (vterm (generate-new-buffer-name name)))))

(defun jps-project-magit ()
  "Open Magit status in the project."
  (interactive)
  (if (require 'magit nil t)
      (let ((default-directory (jps--project-root)))
        (magit-status-setup-buffer default-directory))
    (user-error "Magit not installed")))

(defun jps--file-exists-any (root &rest names)
  (seq-some (lambda (n) (file-exists-p (expand-file-name n root))) names))

(defun jps-project-test ()
  "Smart test command based on project type."
  (interactive)
  (let* ((root (jps--project-root))
         (cmd (cond
               ((file-exists-p (expand-file-name "go.mod" root)) "go test ./...")
               ((file-exists-p (expand-file-name "Cargo.toml" root)) "cargo test")
               ((jps--file-exists-any root "pytest.ini" "pyproject.toml" "tox.ini") "pytest -q")
               ((jps--file-exists-any root "justfile" "Justfile" ".justfile") "just test")
               ((file-exists-p (expand-file-name "Makefile" root)) "make test")
               (t (read-shell-command "Test command: ")))))
    (jps--run-in-project cmd)))

(defun jps-project-build ()
  "Smart build command based on project type."
  (interactive)
  (let* ((root (jps--project-root))
         (cmd (cond
               ((file-exists-p (expand-file-name "go.mod" root)) "go build ./...")
               ((file-exists-p (expand-file-name "Cargo.toml" root)) "cargo build")
               ((jps--file-exists-any root "pyproject.toml" "setup.cfg" "setup.py") "python -m build")
               ((jps--file-exists-any root "justfile" "Justfile" ".justfile") "just build")
               ((file-exists-p (expand-file-name "Makefile" root)) "make")
               (t (read-shell-command "Build command: ")))))
    (jps--run-in-project cmd)))

(defun jps-project-deploy ()
  "Best-guess deploy: justfile deploy > Makefile deploy > scripts/deploy.sh."
  (interactive)
  (let* ((root (jps--project-root))
         (just-files (seq-filter (lambda (f) (file-exists-p (expand-file-name f root)))
                                 '("justfile" "Justfile" ".justfile")))
         (mk (expand-file-name "Makefile" root))
         (script (expand-file-name "scripts/deploy.sh" root))
         (cmd (cond
               ((and just-files
                     (let ((jf (expand-file-name (car just-files) root)))
                       (with-temp-buffer
                         (insert-file-contents jf)
                         (goto-char (point-min))
                         (re-search-forward "^deploy\\s*:" nil t))))
                "just deploy")
               ((and (file-exists-p mk)
                     (with-temp-buffer
                       (insert-file-contents mk)
                       (goto-char (point-min))
                       (re-search-forward "^deploy\\s*:" nil t)))
                "make deploy")
               ((file-exists-p script) "scripts/deploy.sh")
               (t (read-shell-command "Deploy command: ")))))
    (jps--run-in-project cmd)))

(defun jps-project-recompile ()
  "Re-run the last project compile command."
  (interactive)
  (if jps--last-compile-cmd
      (jps--run-in-project jps--last-compile-cmd)
    (user-error "No previous project command")))

(defun jps-project-open-compose ()
  "Open docker compose file in project if present."
  (interactive)
  (let* ((root (jps--project-root))
         (cand (seq-filter
                #'file-exists-p
                (mapcar (lambda (n) (expand-file-name n root))
                        '("docker-compose.yml" "docker-compose.yaml" "compose.yml" "compose.yaml")))))
    (if cand
        (find-file (car cand))
      (user-error "No docker compose file found in project"))))

(defun jps-project-notes ()
  "Open or create project-specific notes file."
  (interactive)
  (let* ((root (jps--project-root))
         (notes-file (expand-file-name "NOTES.org" root)))
    (find-file notes-file)
    (when (not (file-exists-p notes-file))
      (insert (format "#+TITLE: %s Project Notes\n#+DATE: %s\n\n* Tasks\n\n* Ideas\n\n* Issues\n"
                      (file-name-nondirectory (directory-file-name root))
                      (format-time-string "%Y-%m-%d")))
      (save-buffer))))

;; Bind under C-x p …
(define-key project-prefix-map (kbd "R") #'jps-project-ripgrep)
(define-key project-prefix-map (kbd "S") #'jps-project-vterm)
(define-key project-prefix-map (kbd "M") #'jps-project-magit)
(define-key project-prefix-map (kbd "T") #'jps-project-test)
(define-key project-prefix-map (kbd "B") #'jps-project-build)
(define-key project-prefix-map (kbd "D") #'jps-project-deploy)
(define-key project-prefix-map (kbd "C") #'jps-project-open-compose)
(define-key project-prefix-map (kbd "N") #'jps-project-notes)
(define-key project-prefix-map (kbd "r") #'jps-project-recompile)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements
    project-prefix-map
    "R" "ripgrep"
    "S" "shell (vterm)"
    "M" "magit status"
    "T" "test"
    "B" "build"
    "D" "deploy"
    "C" "open compose"
    "N" "project notes"
    "G" "deadgrep"
    "r" "recompile last"))

;;; ============================================================================
;;; Key Bindings (global quality-of-life)
;;; ============================================================================

(global-set-key (kbd "C-c T") (lambda () (interactive) (load-theme 'manoj-dark)))
(global-set-key (kbd "C-c F") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c J") #'jps-json-flatten)
(global-set-key (kbd "C-c L") (lambda () (interactive) (load-theme 'adwaita)))
(global-set-key (kbd "C-c W") #'whitespace-mode)
(global-set-key (kbd "C-c X") #'jps-toggle-transparency)
(global-set-key (kbd "C-c a") #'align-regexp)
(global-set-key (kbd "C-c d") #'jps-kill-line)
(global-set-key (kbd "C-c f d") #'jps-diff-buffer)
(global-set-key (kbd "C-c j") #'jps-json-format)
(global-set-key (kbd "C-c l") #'add-change-log-entry)
(global-set-key (kbd "C-c s") #'sort-lines)
(global-set-key (kbd "C-c t") #'jps-generate-timestamp)
(global-set-key (kbd "C-c w") #'whitespace-cleanup)
(global-set-key (kbd "C-c x") #'jps-configure-platform)

;;; ============================================================================
;;; Additional Configuration
;;; ============================================================================

;; Org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Default theme and UI
(load-theme 'manoj-dark t)
(electric-pair-mode t)
(delete-selection-mode 1)
(setq mouse-yank-at-point t)

;; File management
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      revert-without-query '(".*")
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))
(make-directory "~/.emacs.d/auto-save/" t)

;; Cleanup auto-save files periodically (every 10 minutes; remove older than 7 days)
(run-with-idle-timer
 600 t
 (lambda ()
   (let* ((dir "~/.emacs.d/auto-save/")
          (now (float-time)))
     (when (file-exists-p dir)
       (dolist (f (directory-files dir t "^[^.]"))
         (when (and (file-regular-p f)
                    (> (- now (float-time (file-attribute-modification-time (file-attributes f))))
                       (* 7 24 60 60)))
           (ignore-errors (delete-file f))))))))

;; Session management
(savehist-mode 1)
(save-place-mode 1) ;; reopen files at last cursor pos

;; Consult integration
(with-eval-after-load 'consult
  (setq consult-project-root-function #'project-root))

;; Final touch
(jps-toggle-transparency)

(provide 'jps)
;;; jps.el ends here
