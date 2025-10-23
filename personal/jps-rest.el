;;; jps-rest.el --- REST/API development tools -*- lexical-binding: t -*-
;;; Commentary:
;; Tools for REST API development and testing
;; - restclient-mode for making HTTP requests
;; - Helper functions for token extraction and management
;; - jq integration for JSON processing
;; - API scratchpad per project
;;; Code:

(require 'use-package)
(require 'jps-core)
;; project.el is already loaded by jps-core

;;; ============================================================================
;;; REST/API Development Tools
;;; ============================================================================

(use-package restclient :straight t
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (defun jps-restclient-extract-token ()
    "Extract token from last response and insert/update :token variable."
    (interactive)
    (let ((token nil))
      (save-excursion
        (when (get-buffer "*HTTP Response*")
          (with-current-buffer "*HTTP Response*"
            (goto-char (point-min))
            (when (re-search-forward "\"id_token\"\\s-*:\\s-*\"\\([^\"]+\\)\"" nil t)
              (setq token (match-string 1))))))
      (when token
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^:token = .*$" nil t)
              (replace-match (format ":token = %s" token))
            (goto-char (point-min))
            (insert (format ":token = %s\n\n" token))))
        (message "Token extracted and set!"))
      (unless token (message "No id_token found in response"))))
  :bind (:map restclient-mode-map
              ("C-c C-t" . jps-restclient-extract-token)))

;; REST ergonomics
(use-package company-restclient :straight t)
(use-package jq-mode           :straight t) ; view JSON with jq
(use-package graphql-mode      :straight t)
(use-package ob-restclient     :straight t
  :after (org restclient)
  :init (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))

(with-eval-after-load 'restclient
  (add-hook 'restclient-mode-hook
            (lambda ()
              (setq-local company-backends '((company-restclient company-dabbrev-code)))
              (company-mode 1)
              (visual-line-mode 1))))

(defun jps-project-open-api ()
  "Open or create the project's api.http scratchpad."
  (interactive)
  (let* ((root (jps--project-root))
         (file (expand-file-name "api.http" root)))
    (unless (file-exists-p file)
      (with-temp-file file
        (insert "# Project API scratchpad\n"
                "# Tip: C-c C-c sends the request under point\n\n"
                ":env = dev\n"
                ":base_url = http://localhost:8080\n"
                ":token = \n\n"
                "### Health\nGET :base_url/health\n\n"
                "### Login\nPOST :base_url/login\nContent-Type: application/json\n\n"
                "{\n  \"username\": \"user\",\n  \"password\": \"pass\"\n}\n\n"
                "### Authorized example\nGET :base_url/me\nAuthorization: Bearer :token\n")))
    (find-file file)))

(define-key project-prefix-map (kbd "A") #'jps-project-open-api)
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements project-prefix-map "A" "API scratchpad"))

(defun jps-restclient-set-bearer-from-response ()
  "Grab id_token/access_token from *HTTP Response* and store in Emacs variable.
Detects admin vs contentapi context and stores in appropriate variable."
  (interactive)
  (let ((tok nil)
        (token-type nil))
    (when (get-buffer "*HTTP Response*")
      (with-current-buffer "*HTTP Response*"
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "\"\\(id_token\\|access_token\\|jwt_token\\)\"\\s-*:\\s-*\"\\([^\"]+\\)\"" nil t)
            (setq token-type (match-string 1))
            (setq tok (match-string 2))))))
    (if tok
        (let ((is-admin (or (string-match-p "/admin/" (or (buffer-file-name) ""))
                            (save-excursion
                              (goto-char (point-min))
                              (re-search-forward "admin" nil t)))))
          (if is-admin
              (progn
                (setq jps-rest-admin-token tok)
                (message "Admin token stored (expires in ~1hr). Token type: %s" token-type))
            (progn
              (setq jps-rest-contentapi-token tok)
              (message "Content API token stored. Token type: %s" token-type)))
          ;; Also update the buffer for immediate use
          (save-excursion
            (goto-char (point-min))
            (if (re-search-forward "^:token\\s-*=" nil t)
                (replace-match (format ":token = %s" tok) t t)
              (goto-char (point-min))
              (when (re-search-forward "^###" nil t)
                (beginning-of-line)
                (insert (format ":token = %s\n\n" tok))))))
      (message "No id_token/access_token/jwt_token in *HTTP Response*."))))

(with-eval-after-load 'restclient
  (define-key restclient-mode-map (kbd "C-c C-b") #'jps-restclient-set-bearer-from-response))

(defun jps-restclient-set-env (name)
  "Set :env variable at top of current .http buffer to NAME."
  (interactive (list (completing-read "env: " '("dev" "stage" "prod") nil t)))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^:env\\s-*=" nil t)
        (replace-match (format ":env = %s" name) t t)
      (goto-char (point-min))
      (insert (format ":env = %s\n\n" name))))
  (message "env => %s" name))

(with-eval-after-load 'restclient
  (define-key restclient-mode-map (kbd "C-c C-e") #'jps-restclient-set-env))

(defun jps-restclient-jq (filter)
  "Run jq FILTER over the *HTTP Response* JSON and show in a temp buffer."
  (interactive "sjq filter: ")
  (let ((resp (get-buffer "*HTTP Response*")))
    (unless resp (user-error "No *HTTP Response* buffer"))
    (with-current-buffer resp
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward "^{\\|\\[" nil t)
          (user-error "No JSON payload found in *HTTP Response*")))
      (let* ((json (buffer-substring-no-properties (match-beginning 0) (point-max)))
             (buf (get-buffer-create "*HTTP Response | jq*")))
        (with-current-buffer buf
          (erase-buffer)
          (let* ((tmp (make-temp-file "resp" nil ".json" json))
                 (cmd (format "jq '%s' %s" filter (shell-quote-argument tmp))))
            (call-process-shell-command cmd nil t t)
            (delete-file tmp))
          (jq-mode))
        (pop-to-buffer buf)))))

(with-eval-after-load 'restclient
  (define-key restclient-mode-map (kbd "C-c C-j") #'jps-restclient-jq))

;;; ============================================================================
;;; REST Template Management
;;; ============================================================================

(defvar jps-rest-admin-token nil
  "Current admin JWT token for REST API calls.")

(defvar jps-rest-contentapi-token nil
  "Current content API token/session for REST API calls.")

(defun jps-rest-templates-dir ()
  "Get the templates directory for the current project."
  (let ((root (jps--project-root)))
    (expand-file-name "templates" root)))

(defun jps-rest-list-templates ()
  "List all available REST templates organized by category."
  (interactive)
  (let ((templates-dir (jps-rest-templates-dir)))
    (unless (file-directory-p templates-dir)
      (user-error "Templates directory not found: %s" templates-dir))
    (let* ((categories (directory-files templates-dir t "^[^.]"))
           (templates '()))
      (dolist (category categories)
        (when (file-directory-p category)
          (let ((category-name (file-name-nondirectory category))
                (files (directory-files category t "\\.http$")))
            (dolist (file files)
              (push (cons (format "%s/%s"
                                  category-name
                                  (file-name-sans-extension (file-name-nondirectory file)))
                          file)
                    templates)))))
      (if templates
          (let* ((choices (mapcar #'car (reverse templates)))
                 (choice (completing-read "Load template: " choices nil t)))
            (find-file (cdr (assoc choice templates))))
        (user-error "No templates found in %s" templates-dir)))))

(defun jps-rest-load-template (category name)
  "Load a REST template from CATEGORY with NAME."
  (interactive
   (let* ((templates-dir (jps-rest-templates-dir))
          (categories (when (file-directory-p templates-dir)
                        (directory-files templates-dir nil "^[^.]")))
          (category (completing-read "Category: " categories nil t))
          (category-dir (expand-file-name category templates-dir))
          (templates (when (file-directory-p category-dir)
                       (directory-files category-dir nil "\\.http$")))
          (template-names (mapcar (lambda (f) (file-name-sans-extension f)) templates))
          (name (completing-read "Template: " template-names nil t)))
     (list category name)))
  (let ((file (expand-file-name (format "%s/%s.http" category name)
                                (jps-rest-templates-dir))))
    (if (file-exists-p file)
        (find-file file)
      (user-error "Template not found: %s" file))))

(defun jps-rest-inject-token ()
  "Inject stored token into current buffer's :token variable."
  (interactive)
  (let* ((is-admin (or (string-match-p "/admin/" (or (buffer-file-name) ""))
                       (save-excursion
                         (goto-char (point-min))
                         (re-search-forward "/admin/" nil t))))
         (token (if is-admin jps-rest-admin-token jps-rest-contentapi-token))
         (token-name (if is-admin "admin" "contentapi")))
    (if token
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^:token\\s-*=.*$" nil t)
              (progn
                (replace-match (format ":token = %s" token))
                (message "Injected %s token" token-name))
            (message "No :token variable found in buffer")))
      (message "No %s token stored. Run auth/%s-login.http first." token-name token-name))))

(defun jps-rest-clear-tokens ()
  "Clear all stored REST API tokens."
  (interactive)
  (setq jps-rest-admin-token nil)
  (setq jps-rest-contentapi-token nil)
  (message "All tokens cleared"))

(defun jps-rest-regenerate-templates ()
  "Regenerate REST templates from lambdas.d/ YAML files."
  (interactive)
  (let* ((root (jps--project-root))
         (script (expand-file-name "generate-templates.sh" root))
         (default-directory root))
    (unless (file-exists-p script)
      (user-error "Generator script not found: %s" script))
    (message "Regenerating templates...")
    (let ((output (shell-command-to-string (format "bash %s admin contentapi" script))))
      (message "%s" output)
      (message "Template regeneration complete!"))))

(defun jps-rest-auto-inject-token (&rest _args)
  "Auto-inject stored token before executing request if :token is empty.
This is advice for restclient execution to seamlessly use cached tokens."
  (when (eq major-mode 'restclient-mode)
    (let* ((is-admin (or (string-match-p "/admin/" (or (buffer-file-name) ""))
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward "/admin/" nil t))))
           (token (if is-admin jps-rest-admin-token jps-rest-contentapi-token)))
      (when token
        ;; Check if :token variable exists and is empty
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "^:token\\s-*=\\s-*$" nil t)
            (replace-match (format ":token = %s" token))
            (message "Auto-injected %s token from cache" (if is-admin "admin" "contentapi"))))))))

;; Add advice to auto-inject tokens before request execution
(with-eval-after-load 'restclient
  (advice-add 'restclient-http-send-current-raw :before #'jps-rest-auto-inject-token))

;; Keybindings
(with-eval-after-load 'restclient
  (define-key restclient-mode-map (kbd "C-c C-l") #'jps-rest-list-templates)
  (define-key restclient-mode-map (kbd "C-c C-i") #'jps-rest-inject-token))

(define-key project-prefix-map (kbd "T") #'jps-rest-list-templates)
(define-key project-prefix-map (kbd "R") #'jps-rest-regenerate-templates)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements project-prefix-map
    "T" "REST templates"
    "R" "Regenerate templates"))

(provide 'jps-rest)
;;; jps-rest.el ends here
