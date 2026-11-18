;;; jps-db.el --- Database client (clutch) -*- lexical-binding: t -*-
;;; Commentary:
;; clutch: SQL query consoles, a result grid, staged row edits, ER view.
;; Backends: PostgreSQL (pgsql.el, pure elisp) and SQLite (built in).
;; MySQL, MongoDB, Redis and the JDBC backends are deliberately left out.
;;
;; Connections go in ~/.emacs.d/custom.el with no passwords.  clutch falls
;; back to auth-source, which `jps-op' answers from 1Password:
;;
;;   (setq clutch-connection-alist
;;         '(("app-dev" . (:backend pg :host "localhost" :port 5432
;;                         :user "app" :database "app_dev"))))
;;   (setq jps-op-references
;;         '((("localhost" "app") . "op://Dev/app-db-dev/password")))
;;
;; A project names its connection in .dir-locals.el, and C-x p Q opens a
;; console on it:
;;
;;   ((nil . ((jps-db-connection . "app-dev"))))
;;
;; SQLite needs no connection entry: M-x clutch-query-sqlite-file.
;;; Code:

(require 'use-package)
(require 'jps-core)

(declare-function clutch-query-console "clutch-query" (target))

(use-package clutch :straight t :defer t)
;; Explicit recipe: pgsql is new on MELPA, so older recipe mirrors lack it.
(use-package pgsql
  :straight (:type git :host github :repo "LuciusChen/pgsql.el")
  :defer t)

(defcustom jps-db-connection nil
  "Name of the project's saved clutch connection.
Set it in the project's .dir-locals.el; see `jps-project-db'."
  :type '(choice (const nil) string)
  :safe #'stringp
  :group 'tools)

(defun jps-project-db ()
  "Open a clutch query console on the current project's database.
Uses `jps-db-connection' from the project's dir-locals, or prompts."
  (interactive)
  (require 'clutch)
  (if-let* ((name (jps-project-local 'jps-db-connection)))
      (clutch-query-console name)
    (call-interactively #'clutch-query-console)))

(define-key project-prefix-map (kbd "Q") #'jps-project-db)
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements project-prefix-map "Q" "Query database"))

(provide 'jps-db)
;;; jps-db.el ends here
