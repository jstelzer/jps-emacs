;;; init.el --- Refined config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Bundled libgccjit can misidentify newer Darwin releases as macOS 17/18,
;; which clang rejects.  Use a supported deployment target for native code
;; on macOS 26+ (Darwin 25+), before straight.el or subr trampolines compile.
;; Keep this here because setup.sh currently installs only init.el.
(when (and (eq system-type 'darwin)
           (version<= "25" operating-system-release))
  (setq native-comp-compiler-options '("-mmacosx-version-min=14.0")
        native-comp-driver-options '("-mmacosx-version-min=14.0")))

;; Bootstrap straight.el for package management
;; This is the ONLY package manager we use - no package.el
;; Emacs <29 compat: some packages (e.g. project.el from MELPA) call this.
;; Pick font + size (family must be installed on your system)
(setq package-enable-at-startup nil)
(set-face-attribute 'default nil :family "MesloLGS NF" :height 160)
(unless (fboundp 'define-completion-category)
  (defun define-completion-category (&rest _args) nil))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Activate Org before other packages load any of its libraries.
;; In particular, shell-maker's markdown-overlays requires org-faces.
(straight-use-package 'org)

;; Install and configure use-package with straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Enable lockfile support for reproducible builds across machines
;; This creates straight/versions/default.el which locks package versions
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; JPS customizations - machine-specific settings
;; Custom file will be created automatically by Emacs when you use the customize interface
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; configure my custom load path
(add-to-list 'load-path "~/.emacs.d/personal")
(add-to-list 'load-path "~/.emacs.d/site-lisp")  ; Vendored dependencies

;; Ensure use-package is available before loading personal config
(require 'use-package)

;; A bunch of my macros and stuff I use/key bindings
(require 'jps)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-to-list 'image-types 'svg)

;;; init.el ends here
