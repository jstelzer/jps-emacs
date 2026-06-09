;;; jps-hcl.el --- HCL / Atlas quality-of-life helpers -*- lexical-binding: t -*-
;;; Commentary:
;; Generic HCL editing plus Atlas CLI helpers.
;;; Code:

(require 'use-package)
(require 'compile)
(require 'project)

(use-package hcl-mode
  :straight t
  :mode (("\\.hcl\\'" . hcl-mode))
  :config
  (setq hcl-indent-level 2))

(use-package terraform-mode
  :straight t
  :mode (("\\.tf\\'" . terraform-mode)
         ("\\.tfvars\\'" . terraform-mode))
  :hook ((terraform-mode . outline-minor-mode)
         (hcl-mode . outline-minor-mode)))

(defgroup jps-hcl nil
  "Personal HCL and Atlas helpers."
  :group 'tools)

(defcustom jps-atlas-command "atlas"
  "Atlas CLI executable."
  :type 'string
  :group 'jps-hcl)

(defun jps-hcl--project-root ()
  "Return project root or current directory."
  (if-let ((proj (project-current nil)))
      (project-root proj)
    default-directory))

(defun jps-hcl--compile (cmd)
  "Run CMD from the current project root using `compile'."
  (let ((default-directory (jps-hcl--project-root)))
    (compile cmd)))

(defun jps-atlas-fmt ()
  "Format the current Atlas/HCL file."
  (interactive)
  (if buffer-file-name
      (let ((cmd (format "%s fmt %s"
                         jps-atlas-command
                         (shell-quote-argument buffer-file-name))))
        (jps-hcl--compile cmd))
    (user-error "Buffer is not visiting a file")))

(defun jps-atlas-inspect ()
  "Run Atlas schema inspect using the current project context."
  (interactive)
  (jps-hcl--compile (format "%s schema inspect" jps-atlas-command)))

(defun jps-atlas-migrate-diff ()
  "Run Atlas migrate diff."
  (interactive)
  (jps-hcl--compile (format "%s migrate diff" jps-atlas-command)))

(defun jps-atlas-migrate-lint ()
  "Run Atlas migrate lint."
  (interactive)
  (jps-hcl--compile (format "%s migrate lint" jps-atlas-command)))

(defun jps-atlas-migrate-apply-dry-run ()
  "Run Atlas migrate apply in dry-run mode."
  (interactive)
  (jps-hcl--compile (format "%s migrate apply --dry-run" jps-atlas-command)))

(defun jps-atlas-migrate-status ()
  "Run Atlas migrate status."
  (interactive)
  (jps-hcl--compile (format "%s migrate status" jps-atlas-command)))

(defun jps-hcl-setup-keys ()
  "Set up HCL/Atlas keybindings."
  (local-set-key (kbd "C-c h f") #'jps-atlas-fmt)
  (local-set-key (kbd "C-c h i") #'jps-atlas-inspect)
  (local-set-key (kbd "C-c h d") #'jps-atlas-migrate-diff)
  (local-set-key (kbd "C-c h l") #'jps-atlas-migrate-lint)
  (local-set-key (kbd "C-c h a") #'jps-atlas-migrate-apply-dry-run)
  (local-set-key (kbd "C-c h s") #'jps-atlas-migrate-status))

(add-hook 'hcl-mode-hook #'jps-hcl-setup-keys)
(add-hook 'terraform-mode-hook #'jps-hcl-setup-keys)

(provide 'jps-hcl)
;;; jps-hcl.el ends here
