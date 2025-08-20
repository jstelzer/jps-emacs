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
  ;; Set default font
  (set-face-attribute 'default nil
                      :family "MesloLGS NF"
                      :height 120))

;; Start server only in interactive mode
(unless noninteractive
  (server-start))

;; Personal information
(setq add-log-mailing-address "mental@neverlight.com"
      add-log-full-name "Jason Stelzer")

;; Python configuration
(setq py-python-command "ipython"
      py-python-command-args '("--pylab" "inline" "--colors" "NoColor"))

;; File handling
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(global-font-lock-mode t)

;; Interface improvements
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
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

;; (defun jps-json-flatten (b e)
;;   "Flatten region B..E into one line of JSON."
;;   (interactive "r")
;;   (shell-command-on-region b e "perl -pe 's/\\n//g; s/\\s+/ /g;'" (current-buffer) t))

(defun jps-generate-timestamp ()
  "Insert a timestamp at point."
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

(defun jps-toggle-transparency ()
  "Toggle window transparency on/off."
  (interactive)
  (let ((current (frame-parameter nil 'alpha-background)))
    (if (and current (< current 100))
        (set-frame-parameter nil 'alpha-background 100)
      (set-frame-parameter nil 'alpha-background 75))))

(defun jps-configure-platform ()
  "Configure platform-specific options."
  (interactive)
  (when (eq system-type 'darwin)
    (message "Configuring macOS...")
    (when (display-graphic-p)
      (setq default-input-method "MacOSX")
      ;; Smooth scrolling for trackpad/mouse
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
;; Bigger pipe for language servers (Emacs 27+)
(setq read-process-output-max (* 1024 1024)) ; 1MB
;; Less chatty echo area from Eglot
(with-eval-after-load 'eglot
  (setq eglot-events-buffer-size 0))

(use-package eglot
  :straight t
  :hook ((rust-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (python-mode . eglot-ensure)))

;; Docker/YAML modes
(use-package dockerfile-mode
  :straight t)
(use-package yaml-mode
  :straight t)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\`Dockerfile\\'" . dockerfile-mode))

;; Go
(use-package go-mode
  :straight t
  :hook (before-save . gofmt-before-save))

;; Rust
(use-package rust-mode
  :straight t)
(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))
(use-package rust-playground
  :straight t)

;; Python venv/format
(use-package pyvenv
  :straight t
  :config (pyvenv-mode 1))
(use-package blacken
  :straight t
  :hook (python-mode . blacken-mode))

;; Code quality & completion
(use-package flycheck
  :straight t
  :init (global-flycheck-mode))
(use-package yasnippet
  :straight t
  :config (yas-global-mode))
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

;; Completion stack
(use-package vertico
  :straight t
  :init (vertico-mode 1))
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :straight t
  :init (marginalia-mode 1))

;; Consult — no M-p override; keep stock project keys
(use-package consult
  :straight t
  :bind (("C-S-p" . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :init
  (setq consult-find-command
        "fd --hidden --follow --exclude .git --color=never -t f \\S- || find . -type f"))

;; Embark
(use-package embark
  :straight t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Project: use the stock C-x p prefix (Emacs 28+)
(use-package project
  :straight nil
  :init
  (setq project-vc-extra-root-markers '(".git")))

;; Treemacs
(use-package treemacs
  :straight t
  :bind (("C-x t t" . treemacs)
         ("C-x t 1" . treemacs-delete-other-windows)
         ("C-x t b" . treemacs-bookmark))
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-git-mode 'deferred)
  (when (fboundp 'treemacs-hide-gitignored-files-mode)
    (treemacs-hide-gitignored-files-mode 1)))

(use-package treemacs-project-follow-mode
  :straight nil
  :after treemacs
  :config (treemacs-project-follow-mode 1))

;; Which-Key
(use-package which-key
  :straight t
  :init (which-key-mode 1)
  :custom (which-key-idle-delay 0.4))

;;; ============================================================================
;;; External Tools Integration
;;; ============================================================================

(use-package vterm
  :straight t)

;; Git integration
(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

(use-package git-gutter
  :straight t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

;; Search and navigation
(use-package deadgrep
  :straight t
  :bind ("C-x p G" . deadgrep))

;; REST API client
(use-package restclient
  :straight t
  :mode ("\\.http\\'" . restclient-mode))

;; Docker management
(use-package docker
  :straight t
  :bind ("C-c D" . docker))

;; Multiple cursors for refactoring
(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

;; Markdown support
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Claude Code IDE integration
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;;; ============================================================================
;;; Project Dashboard (C-x p …)
;;; ============================================================================

(require 'project)
(defvar jps--last-compile-cmd nil)

(defun jps--project-root ()
  (when-let* ((proj (project-current t)))
    (expand-file-name (project-root proj))))

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
         (cmd
          (cond
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
         (cmd
          (cond
           ((file-exists-p (expand-file-name "go.mod" root)) "go build ./...")
           ((file-exists-p (expand-file-name "Cargo.toml" root)) "cargo build")
           ((jps--file-exists-any root "pyproject.toml" "setup.cfg" "setup.py") "python -m build")
           ((jps--file-exists-any root "justfile" "Justfile" ".justfile") "just build")
           ((file-exists-p (expand-file-name "Makefile" root)) "make")
           (t (read-shell-command "Build command: ")))))
    (jps--run-in-project cmd)))

(defun jps-project-deploy ()
  "Best-guess deploy: prefer justfile 'deploy', then Makefile 'deploy', then scripts/deploy.sh."
  (interactive)
  (let* ((root (jps--project-root))
         (just-files (seq-filter 
                      (lambda (f) (file-exists-p (expand-file-name f root)))
                      '("justfile" "Justfile" ".justfile")))
         (mk (expand-file-name "Makefile" root))
         (script (expand-file-name "scripts/deploy.sh" root))
         (cmd (cond
               ;; Check for justfile with deploy recipe
               ((and just-files
                     (let ((jf (expand-file-name (car just-files) root)))
                       (with-temp-buffer
                         (insert-file-contents jf)
                         (goto-char (point-min))
                         (re-search-forward "^deploy\\s*:" nil t))))
                "just deploy")
               ;; Check for Makefile with deploy target
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

;; Bind under C-x p … (use uppercase to avoid clobbering stock keys)
(define-key project-prefix-map (kbd "R") #'jps-project-ripgrep)   ;; R = Ripgrep
(define-key project-prefix-map (kbd "S") #'jps-project-vterm)     ;; S = Shell (vterm)
(define-key project-prefix-map (kbd "M") #'jps-project-magit)     ;; M = Magit
(define-key project-prefix-map (kbd "T") #'jps-project-test)      ;; T = Test
(define-key project-prefix-map (kbd "B") #'jps-project-build)     ;; B = Build
(define-key project-prefix-map (kbd "D") #'jps-project-deploy)    ;; D = Deploy
(define-key project-prefix-map (kbd "C") #'jps-project-open-compose) ;; C = Compose
(define-key project-prefix-map (kbd "N") #'jps-project-notes)     ;; N = Notes
(define-key project-prefix-map (kbd "r") #'jps-project-recompile) ;; r = re-run last compile


;; Which-Key labels for the dashboard
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

;; Cleanup auto-save files periodically (every 10 minutes, nuke contents)
(run-with-idle-timer
 600 t
 (lambda ()
   (let* ((dir "~/.emacs.d/auto-save/")
          (now (float-time)))
     (when (file-exists-p dir)
       (dolist (f (directory-files dir t "^[^.]"))
         (when (and (file-regular-p f)
                    (> (- now (float-time (file-attribute-modification-time (file-attributes f))))
                       (* 7 24 60 60))) ; older than 7 days
           (ignore-errors (delete-file f))))))))

;; Session management
(savehist-mode 1)
;; Make TABs consistent in views that still show them
(setq-default tab-width 4)

;; Yank/kill ring history survives longer Emacs sessions
(save-place-mode 1)  ;; reopen files at last cursor pos

;; Go: use goimports instead of gofmt (adds missing imports on save)
(with-eval-after-load 'go-mode
  (setq gofmt-command "goimports"))

;; Rust: format on save via rustfmt
(with-eval-after-load 'rust-mode
  (setq rust-format-on-save t))
(with-eval-after-load 'consult
  (setq consult-project-root-function #'project-root))
(with-eval-after-load 'project
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


(jps-toggle-transparency)
(provide 'jps)
;;; jps.el ends here
