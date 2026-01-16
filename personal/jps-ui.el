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

(use-package corfu
  :straight t
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)                 ; Enable auto completion
  (corfu-cycle t)                 ; Cycle through candidates
  (corfu-auto-delay 0.2)          ; Delay before popup
  (corfu-auto-prefix 2)           ; Min chars before popup
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil))    ; Don't preview current candidate

(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev))

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

;; Icons for dashboard, treemacs, etc.
(use-package all-the-icons :straight t
  :if (display-graphic-p)
  :config
  (let ((guard-file (expand-file-name "~/.emacs.d/.all-the-icons-installed")))
    (unless (file-exists-p guard-file)
      (all-the-icons-install-fonts t)  ; t = non-interactive
      (write-region "" nil guard-file))))

;; Dashboard startup screen
(setq dashboard-items '((recents   . 5)
                        (bookmarks . 5)
                        (projects  . 5)
                        (agenda    . 5)
                        (registers . 5)))

(setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
(setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setopt dashboard-set-heading-icons t
          dashboard-set-file-icons t
          dashboard-banner-logo-title "There is no knowledge that is not power."
          dashboard-center-content t
          dashboard-image-banner-max-height 300
          dashboard-startup-banner "~/.emacs.d/logos/emacs-logo-stranger-things.png"))

;; Consult integration with project.el
(with-eval-after-load 'consult
  (setq consult-project-root-function #'project-root))


(use-package pulsar
  :ensure t
  :bind
  ( :map global-map
    ("C-x l" . pulsar-pulse-line) ; overrides `count-lines-page'
    ("C-x L" . pulsar-highlight-permanently-dwim)) ; or use `pulsar-highlight-temporarily-dwim'
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta))

(provide 'jps-ui)
;;; jps-ui.el ends here
