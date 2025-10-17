;;; jps-project.el --- Smart project management commands -*- lexical-binding: t -*-
;;; Commentary:
;; Project-aware commands for test/build/deploy
;; Intelligently detects project type and runs appropriate commands
;;; Code:

(require 'use-package)
(require 'jps-core)
;; project.el is already loaded by jps-core

;;; ============================================================================
;;; Project Dashboard (C-x p …)
;;; ============================================================================

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

(defun jps-project-test ()
  "Smart test command based on project type."
  (interactive)
  (let* ((root (jps--project-root))
         (cmd (cond
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
         (cmd (cond
               ((file-exists-p (expand-file-name "go.mod" root)) "go build ./...")
               ((file-exists-p (expand-file-name "Cargo.toml" root)) "cargo build")
               ((jps--file-exists-any root "pyproject.toml" "setup.cfg" "setup.py") "python -m build")
               ((jps--file-exists-any root "justfile" "Justfile" ".justfile") "just build")
               ((file-exists-p (expand-file-name "Makefile" root)) "make")
               (t (read-shell-command "Build command: ")))))
    (jps--run-in-project cmd)))

(defun jps-project-deploy ()
  "Best-guess deploy: justfile deploy > Makefile deploy > scripts/deploy.sh."
  (interactive)
  (let* ((root (jps--project-root))
         (just-files (seq-filter (lambda (f) (file-exists-p (expand-file-name f root)))
                                 '("justfile" "Justfile" ".justfile")))
         (mk (expand-file-name "Makefile" root))
         (script (expand-file-name "scripts/deploy.sh" root))
         (cmd (cond
               ((and just-files
                     (let ((jf (expand-file-name (car just-files) root)))
                       (with-temp-buffer
                         (insert-file-contents jf)
                         (goto-char (point-min))
                         (re-search-forward "^deploy\\s*:" nil t))))
                "just deploy")
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

;; Bind under C-x p …
(define-key project-prefix-map (kbd "R") #'jps-project-ripgrep)
(define-key project-prefix-map (kbd "S") #'jps-project-vterm)
(define-key project-prefix-map (kbd "M") #'jps-project-magit)
(define-key project-prefix-map (kbd "T") #'jps-project-test)
(define-key project-prefix-map (kbd "B") #'jps-project-build)
(define-key project-prefix-map (kbd "D") #'jps-project-deploy)
(define-key project-prefix-map (kbd "C") #'jps-project-open-compose)
(define-key project-prefix-map (kbd "N") #'jps-project-notes)
(define-key project-prefix-map (kbd "r") #'jps-project-recompile)

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

(provide 'jps-project)
;;; jps-project.el ends here
