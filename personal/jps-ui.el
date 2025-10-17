;;; jps-ui.el --- Modern Emacs UI packages -*- lexical-binding: t -*-
;;; Commentary:
;; Vertico for vertical completion UI
;; Orderless for flexible matching
;; Marginalia for rich annotations
;; Consult for enhanced commands
;; Embark for context actions
;; Treemacs for file tree sidebar
;; Which-key for keybinding help
;;; Code:

(require 'use-package)
;; project.el is loaded by jps-core

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

;; Project configuration (project.el loaded by jps-core)
(with-eval-after-load 'project
  (setq project-vc-extra-root-markers '(".git")
        project-switch-commands
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

;; Consult integration with project.el
(with-eval-after-load 'consult
  (setq consult-project-root-function #'project-root))

(provide 'jps-ui)
;;; jps-ui.el ends here
