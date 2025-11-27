;;; jps-core.el --- Core utilities and configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Foundation layer: basic configuration, utility functions, and shared helpers
;; This module has no dependencies on other jps-* modules
;;; Code:

(require 'use-package)
(require 'map)
;(require 'transient)
;; Make sure transient is actually loaded at runtime
(use-package transient
  :straight t
  :demand t)

;; Explicitly prevent straight from managing project.el (use built-in only)
(when (boundp 'straight-built-in-pseudo-packages)
  (add-to-list 'straight-built-in-pseudo-packages 'project))

;; Use built-in project.el
(require 'project)

;;; ============================================================================
;;; Basic Configuration
;;; ============================================================================

;; Only do this on Emacs 29+
(when (fboundp 'define-completion-category)
  (define-completion-category
    'project-buffer '(buffer)
    "Completion category for buffers in a given project."))
;; For Emacs <29 you can still tweak styles via overrides, e.g.:
(add-to-list 'completion-category-overrides
             '(project-buffer (styles . (basic substring))))

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
;;; Custom Utility Functions
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
;;; Shared Helper Functions (used by multiple modules)
;;; ============================================================================

(defvar jps--last-compile-cmd nil
  "Last compile command run in a project.")

(defun jps--project-root ()
  "Return the root directory of the current project."
  (when-let* ((proj (project-current t)))
    (expand-file-name (project-root proj))))

(defun jps--read-file-first-line (f)
  "Read the first line of file F, returning it trimmed."
  (when (file-readable-p f)
    (with-temp-buffer
      (insert-file-contents f)
      (string-trim (buffer-substring-no-properties (point-min) (line-end-position))))))

(defun jps--file-exists-any (root &rest names)
  "Check if any file in NAMES exists in ROOT directory."
  (seq-some (lambda (n) (file-exists-p (expand-file-name n root))) names))

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
;;; Global Key Bindings
;;; ============================================================================

(global-set-key (kbd "C-c F") #'toggle-frame-fullscreen)
(global-set-key (kbd "C-c J") #'jps-json-flatten)
(global-set-key (kbd "C-c L") (lambda () (interactive) (load-theme 'adwaita)))
(global-set-key (kbd "C-c T") (lambda () (interactive) (load-theme 'manoj-dark)))
(global-set-key (kbd "C-c W") #'whitespace-mode)
(global-set-key (kbd "C-c X") #'jps-toggle-transparency)
(global-set-key (kbd "C-c A") #'align-regexp)
(global-set-key (kbd "C-c k") #'jps-kill-line)
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

;; Final transparency and font setup
(jps-toggle-transparency)
(set-face-attribute 'default nil :family "MesloLGS NF" :height 160)

(provide 'jps-core)
;;; jps-core.el ends here
