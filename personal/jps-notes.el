;;; jps-notes.el --- Org-roam knowledge management -*- lexical-binding: t -*-
;;; Commentary:
;; Org-roam configuration for linked note-taking and knowledge management
;; Primarily used for extracting/organizing the Forge design corpus into
;; atomic concept nodes for book development.
;;
;; Directory structure:
;;   ~/notes/           - Main org-roam directory
;;     journal/         - Daily notes (auto-created)
;;     concepts/        - Extracted Forge concepts (agents, registers, GBNF, etc.)
;;     meta/            - Meta-notes about the corpus/book structure
;;
;; Key bindings:
;;   C-c n f  - Find/create note (org-roam-node-find)
;;   C-c n i  - Insert link to note (org-roam-node-insert)
;;   C-c n c  - Capture quick note (org-roam-capture)
;;   C-c n j  - Today's journal entry (org-roam-dailies-capture-today)
;;   C-c n b  - Show backlinks buffer (org-roam-buffer-toggle)
;;   C-c n g  - Show graph (org-roam-graph)
;;
;;; Code:

;;; ============================================================================
;;; Package Setup
;;; ============================================================================

(use-package org-roam
  :ensure t
  :defer t  ; Lazy-load until needed
  :commands (org-roam-node-find
             org-roam-node-insert
             org-roam-capture
             org-roam-dailies-capture-today
             org-roam-buffer-toggle
             org-roam-graph)
  :custom
  ;; Core directories (relative to umbrella root, symlinked in setup)
  (org-roam-directory (expand-file-name "~/notes"))
  (org-roam-dailies-directory "journal/")

  ;; Database location (SQLite)
  (org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))

  ;; Completion interface
  (org-roam-completion-everywhere t)

  ;; Graph visualization (requires graphviz)
  (org-roam-graph-executable "dot")
  (org-roam-graph-viewer "firefox")

  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n g" . org-roam-graph))

  :init
  ;; Only create directories if org-roam-directory exists (platform-dependent)
  ;; This prevents errors on macOS where ~/notes won't be symlinked
  (when (file-directory-p (expand-file-name "~/notes"))
    (let* ((notes-dir (expand-file-name "~/notes"))
           (journal-dir (expand-file-name "journal" notes-dir))
           (concepts-dir (expand-file-name "concepts" notes-dir))
           (meta-dir (expand-file-name "meta" notes-dir)))
      (unless (file-directory-p journal-dir) (make-directory journal-dir t))
      (unless (file-directory-p concepts-dir) (make-directory concepts-dir t))
      (unless (file-directory-p meta-dir) (make-directory meta-dir t))))

  :config
  ;; Enable automatic database sync (only when org-roam loads)
  (org-roam-db-autosync-mode))

;;; ============================================================================
;;; Org-roam UI (Live Graph Visualization)
;;; ============================================================================

;; Dependencies for org-roam-ui (must be loaded first)
(use-package websocket
  :straight t)

(use-package simple-httpd
  :straight t
  :ensure nil)  ; Don't try to load "simple-httpd" feature - it doesn't exist

(use-package f
  :straight t)

(use-package org-roam-ui
  :straight t
  :after (org-roam websocket f)
  :custom
  (org-roam-ui-sync-theme t)          ; Match Emacs theme
  (org-roam-ui-follow t)              ; Follow current note in graph
  (org-roam-ui-update-on-save t)      ; Live updates on save
  (org-roam-ui-open-on-start nil)     ; Don't auto-open browser
  :bind (("C-c n u" . org-roam-ui-mode)
         ("C-c n U" . org-roam-ui-open)))  ; Direct browser open

;;; ============================================================================
;;; Capture Templates
;;; ============================================================================

(with-eval-after-load 'org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

          ("c" "concept" plain
           (file "~/.emacs.d/templates/concept-node.org")
           :target (file+head "concepts/${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :concept:\n\n")
           :unnarrowed t)

          ("m" "meta" plain "%?"
           :target (file+head "meta/${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :meta:\n\n")
           :unnarrowed t)))

  ;; Daily note template
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d %A>\n#+date: %U\n\n")))))

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun jps/org-roam-forge-ref (filepath)
  "Create an org-roam reference link to a forge/ file.
FILEPATH should be relative to the forge/ directory."
  (interactive "sForge file path: ")
  (let* ((forge-root (expand-file-name "../forge" user-emacs-directory))
         (full-path (expand-file-name filepath forge-root))
         (link (format "[[file:%s][%s]]" full-path (file-name-nondirectory filepath))))
    (insert link)))

(defun jps/org-roam-concept-skeleton ()
  "Insert skeleton structure for a Forge concept node."
  (interactive)
  (insert "* Overview\n\n")
  (insert "* Definition\n\n")
  (insert "* Related Concepts\n\n")
  (insert "* Source References\n\n")
  (insert "* Book Implications\n\n"))

;;; ============================================================================
;;; UI Enhancements
;;; ============================================================================

;; Show backlinks in side buffer
(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 0.33)
               (window-parameters . ((no-other-window . t)
                                     (no-delete-other-windows . t)))))

(message "JPS notes configuration loaded (org-roam)")

(provide 'jps-notes)
;;; jps-notes.el ends here
