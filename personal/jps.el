;;; jps.el --- Personal Emacs configuration
;;; Commentary:
;; Personal customizations and functions for Emacs
;;; Code:
;; Dunno why this isn't default, but...
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Basic settings
(when (display-graphic-p)
  (tool-bar-mode -1))

;; Start server only in interactive mode
(unless noninteractive
  (server-start))
(setq add-log-mailing-address "mental@neverlight.com")
(setq add-log-full-name "Jason Stelzer")
(setq py-python-command "ipython")
(setq py-python-command-args '("--pylab"  "inline" "--colors" "NoColor"))

;; I hate *~ files scattered all over the place.
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(global-font-lock-mode t)

;; Auto-complete - skip for now to test basic configuration
;; (use-package auto-complete
;;   :defer t
;;   :init
;;   ;; Only enable in interactive mode to avoid batch mode issues
;;   (unless noninteractive
;;     (run-with-idle-timer 1 nil (lambda () (global-auto-complete-mode +1)))))

(fset 'yes-or-no-p 'y-or-n-p)
; spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq c-default-style "user" c-basic-offset 4)

;; Custom functions
(defun jps-kill-line ()
  "Emulate vi's 'dd' command"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (delete-blank-lines))

(defun jps-diff-buffer ()
  "Show the difference between the current buffer and the file on disk"
  (interactive)
  (diff-buffer-with-file (current-buffer)))

; keep PATH and exec-path in sync
(defun jps-add-path (path-element)
  "Add the specified path element to the PATH and exec-path"
  (interactive)
  (if (file-directory-p path-element)
      ; avoid adding the same directory if its already there.
      (if (not ( string-match path-element ( getenv "PATH")))
          ((lambda () (setenv "PATH"
                               (concat (expand-file-name path-element)
                                       path-separator (getenv "PATH")))
                       (setq exec-path (parse-colon-path (getenv "PATH"))))))))

;; Platform-specific configurations
(defun jps-configure-os-x ()
  "Configure OS X specific options"
  (interactive)
  (message "Configuring OS X...")
  (when (display-graphic-p)
    (setq default-input-method "MacOSX")
    (set-frame-parameter nil 'alpha '(80 80))
    
    ; scrolling tweaks for GUI
    (global-set-key [wheel-down] #'(lambda ()
                                    (interactive)
                                    (scroll-down 1)))
    (global-set-key [triple-wheel-down] #'(lambda ()
                                           (interactive)
                                           (scroll-down 1)))
    (global-set-key [double-wheel-down] #'(lambda ()
                                           (interactive)
                                           (scroll-down 1)))
    (global-set-key [triple-wheel-up] #'(lambda ()
                                         (interactive)
                                         (scroll-up 1)))
    (global-set-key [double-wheel-up] #'(lambda ()
                                         (interactive)
                                         (scroll-up 1)))
    (global-set-key [wheel-up] #'(lambda ()
                                  (interactive)
                                  (scroll-up 1))))
  
  (jps-add-path "/usr/local/bin")
  (if (file-directory-p "/usr/local/texlive/2015/bin/x86_64-darwin")
      (jps-add-path "/usr/local/texlive/2015/bin/x86_64-darwin")))

(defun jps-turn-off-os-x-opacity ()
  "Turn off macOS window transparency"
  (interactive)
  (set-frame-parameter nil 'alpha '(100)))

;; Apply platform-specific configuration
(cond
 ((string= "darwin" system-type)
  (jps-configure-os-x))
 (t
  (message "Non-mac platform detected")))

;; Utility functions
(defun jps-json-format(b e)
  "Format the selected region json string via python json.tool"
  (interactive "r")
  (shell-command-on-region b e "python -m json.tool" (current-buffer) t))

(defun jps-json-flatten (b e)
  "Flatten the selected region into one line of json"
  (interactive "r")
  (shell-command-on-region b e "perl -i -pe 's/\n//g; s/\s+/ /g;'" (current-buffer) t))

(defun jps-generate-timestamp ()
  "Insert a date string"
  (interactive)
  (insert (format-time-string "%a %b %d %H:%M:%S %Y")))

; Add home bin directories to PATH
(setq jps-home-bin (concat (getenv "HOME") "/bin"))
(if (file-directory-p jps-home-bin)
    (jps-add-path jps-home-bin))

(setq jps-cargo-bin (concat(getenv "HOME") "/.cargo/bin"))
(if (file-directory-p jps-cargo-bin)
    (jps-add-path jps-cargo-bin))

;; LSP and Go mode setup
(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         (before-save . gofmt-before-save)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-go-use-gopls t))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (require 'dap-go))

;; Company mode for completion
(use-package company
  :defer 0.1
  :config
  (global-company-mode t)
  (setq-default
   company-idle-delay 0.05
   company-require-match nil
   company-minimum-prefix-length 0
   company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)))

;; Terminal emulator
(use-package vterm)

;; Claude Code IDE integration
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

;; Modern completion UI
(use-package vertico
  :init (vertico-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :bind (("M-p" . project-find-file)
         ("C-S-p" . consult-ripgrep)
         ("C-x b" . consult-buffer))
  :init
  (setq consult-find-command "fd --hidden --follow --exclude .git --color=never -t f \\S- || find . -type f"))

;; Project management (built-in, no need to install)
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

;; Context actions
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Key discovery
(use-package which-key
  :init (which-key-mode 1)
  :custom (which-key-idle-delay 0.4))

;; Key bindings
(global-set-key "\C-cD" (lambda () (interactive) (load-theme 'manoj-dark)))
(global-set-key "\C-cF" 'toggle-frame-fullscreen)
(global-set-key "\C-cJ" 'jps-json-flatten)
(global-set-key "\C-cL" (lambda () (interactive) (load-theme 'adwaita)))
(global-set-key "\C-cW" 'whitespace-mode)
(global-set-key "\C-cX" 'jps-turn-off-os-x-opacity)
(global-set-key "\C-ca" 'align-regexp)
(global-set-key "\C-cd" 'jps-kill-line)
(global-set-key "\C-cfd" 'jps-diff-buffer)
(global-set-key "\C-cj" 'jps-json-format)
(global-set-key "\C-cl" 'add-change-log-entry)
(global-set-key "\C-cs" 'sort-lines)
(global-set-key "\C-ct" 'jps-generate-timestamp)
(global-set-key "\C-cw" 'whitespace-cleanup)
(global-set-key "\C-cx" 'jps-configure-os-x)

; org mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)

;; Default theme
(load-theme 'manoj-dark t)

;; Mouse and electric pair mode
(defun track-mouse (e))
(setq mouse-sel-mode t)
(electric-pair-mode t)

; IRC config
(setq rcirc-server-alist '(
                           ("irc.oftc.net"
                            :nick "Fah"
                            :username "Fah"
                            :encryption tls
                            :port 6697)))

;; Auto-revert configuration
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq revert-without-query '(".*"))

;; Auto-save configuration
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))

(make-directory "~/.emacs.d/auto-save/" t)

;; Cleanup old auto-save files periodically
(run-with-idle-timer
 600 t ;; every 10 minutes
 (lambda ()
   (let ((dir "~/.emacs.d/auto-save/"))
     (when (file-exists-p dir)
       (mapc #'delete-file
             (directory-files dir t "^[^.]"))))))

;; Marginalia for better file annotations
(use-package marginalia
  :init (marginalia-mode 1))

;; Keep minibuffer history across sessions
(savehist-mode 1)

(provide 'jps)
;;; jps.el ends here
