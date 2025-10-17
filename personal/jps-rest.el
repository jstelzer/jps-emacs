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
  "Grab id_token/access_token from *HTTP Response* and set :token in current .http buffer."
  (interactive)
  (let ((tok nil))
    (when (get-buffer "*HTTP Response*")
      (with-current-buffer "*HTTP Response*"
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "\"\\(id_token\\|access_token\\)\"\\s-*:\\s-*\"\\([^\"]+\\)\"" nil t)
            (setq tok (match-string 2))))))
    (if tok
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^:token\\s-*=" nil t)
              (replace-match (format ":token = %s" tok) t t)
            (goto-char (point-min))
            (insert (format ":token = %s\n" tok) "\n"))
          (message "Bearer :token updated."))
      (message "No id_token/access_token in *HTTP Response*."))))

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

(provide 'jps-rest)
;;; jps-rest.el ends here
