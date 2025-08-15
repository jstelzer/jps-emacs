;;; jps.el --- Personal Emacs configuration
;;; Commentary:
;; Personal customizations and functions for Emacs
;;; Code:

;;; ============================================================================
;;; Basic Configuration
;;; ============================================================================

;; PATH synchronization (essential for external tools)
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Basic UI settings
(when (display-graphic-p)
  (tool-bar-mode -1))

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
  "Add the specified path element to the PATH and exec-path."
  (interactive)
  (when (and (file-directory-p path-element)
             (not (string-match path-element (getenv "PATH"))))
    (setenv "PATH" (concat (expand-file-name path-element)
                           path-separator (getenv "PATH")))
    (setq exec-path (parse-colon-path (getenv "PATH")))))

(defun jps-json-format (b e)
  "Format the selected region as JSON using python json.tool."
  (interactive "r")
  (shell-command-on-region b e "python -m json.tool" (current-buffer) t))

(defun jps-json-flatten (b e)
  "Flatten the selected region into one line of JSON."
  (interactive "r")
  (shell-command-on-region b e "perl -i -pe 's/\\n//g; s/\\s+/ /g;'" (current-buffer) t))

(defun jps-generate-timestamp ()
  "Insert a timestamp at point."
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

(defun jps-turn-off-os-x-opacity ()
  "Turn off macOS window transparency."
  (interactive)
  (set-frame-parameter nil 'alpha '(100)))

(defun jps-configure-os-x ()
  "Configure macOS-specific options."
  (interactive)
  (message "Configuring macOS...")
  (when (display-graphic-p)
    (setq default-input-method "MacOSX")
    (set-frame-parameter nil 'alpha '(80 80))
    
    ;; Smooth scrolling for trackpad/mouse
    (dolist (event '([wheel-down] [double-wheel-down] [triple-wheel-down]))
      (global-set-key event (lambda () (interactive) (scroll-down 1))))
    (dolist (event '([wheel-up] [double-wheel-up] [triple-wheel-up]))
      (global-set-key event (lambda () (interactive) (scroll-up 1)))))
  
  (jps-add-path "/usr/local/bin"))

;;; ============================================================================
;;; Platform Configuration
;;; ============================================================================

(cond
 ((eq system-type 'darwin)
  (jps-configure-os-x))
 (t
  (message "Non-macOS platform detected")))

;; Add home bin directories to PATH
(dolist (path (list (concat (getenv "HOME") "/bin")
                    (concat (getenv "HOME") "/.cargo/bin")))
  (when (file-directory-p path)
    (jps-add-path path)))

;;; ============================================================================
;;; Development Environment
;;; ============================================================================

;; Language Server Protocol via Eglot
(use-package eglot
  :hook ((rust-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (python-mode . eglot-ensure)))

;; Go development
(use-package go-mode
  :hook (before-save . gofmt-before-save))

;; Rust development
(use-package rust-mode)
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))
(use-package rust-playground)

;; Python development
(use-package pyvenv
  :config (pyvenv-mode 1))
(use-package blacken
  :hook (python-mode . blacken-mode))

;; Code quality and completion
(use-package flycheck
  :init (global-flycheck-mode))

(use-package yasnippet
  :config (yas-global-mode))

(use-package company
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

;; Completion framework
(use-package vertico
  :init (vertico-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :init (marginalia-mode 1))

;; Enhanced navigation and search
(use-package consult
  :bind (("M-p" . project-find-file)
         ("C-S-p" . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :init
  (setq consult-find-command "fd --hidden --follow --exclude .git --color=never -t f \\S- || find . -type f"))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Project management
(use-package project
  :straight nil
  :init
  (setq project-vc-extra-root-markers '(".git")))

;; File tree
(use-package treemacs
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

;; Key discovery and help
(use-package which-key
  :init (which-key-mode 1)
  :custom (which-key-idle-delay 0.4))

;;; ============================================================================
;;; External Tools Integration
;;; ============================================================================

;; Terminal emulator
(use-package vterm)

;; Claude Code IDE integration
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;;; ============================================================================
;;; Key Bindings
;;; ============================================================================

(global-set-key (kbd "C-c D") (lambda () (interactive) (load-theme 'manoj-dark)))
(global-set-key (kbd "C-c F") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c J") #'jps-json-flatten)
(global-set-key (kbd "C-c L") (lambda () (interactive) (load-theme 'adwaita)))
(global-set-key (kbd "C-c W") #'whitespace-mode)
(global-set-key (kbd "C-c X") #'jps-turn-off-os-x-opacity)
(global-set-key (kbd "C-c a") #'align-regexp)
(global-set-key (kbd "C-c d") #'jps-kill-line)
(global-set-key (kbd "C-c f d") #'jps-diff-buffer)
(global-set-key (kbd "C-c j") #'jps-json-format)
(global-set-key (kbd "C-c l") #'add-change-log-entry)
(global-set-key (kbd "C-c s") #'sort-lines)
(global-set-key (kbd "C-c t") #'jps-generate-timestamp)
(global-set-key (kbd "C-c w") #'whitespace-cleanup)
(global-set-key (kbd "C-c x") #'jps-configure-os-x)

;;; ============================================================================
;;; Additional Configuration
;;; ============================================================================

;; Org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Default theme and UI
(load-theme 'manoj-dark t)
(electric-pair-mode t)
(setq mouse-sel-mode t)

;; File management
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t
      revert-without-query '(".*")
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))

(make-directory "~/.emacs.d/auto-save/" t)

;; Cleanup old auto-save files periodically
(run-with-idle-timer
 600 t  ;; Every 10 minutes
 (lambda ()
   (let ((dir "~/.emacs.d/auto-save/"))
     (when (file-exists-p dir)
       (mapc #'delete-file
             (directory-files dir t "^[^.]"))))))

;; Session management
(savehist-mode 1)

(provide 'jps)
;;; jps.el ends here
