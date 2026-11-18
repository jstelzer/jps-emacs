;;; jps-op.el --- 1Password secrets via the op CLI -*- lexical-binding: t -*-
;;; Commentary:
;; Secrets live in 1Password and are read with `op'.  Two pieces:
;;
;; `jps-op-read' resolves an op:// secret reference to its value.  Call it
;; wherever a secret is needed at use time, e.g. an API-key function.
;;
;; An auth-source backend answers lookups for the hosts listed in
;; `jps-op-references', so any auth-source consumer (clutch, forge,
;; smtpmail, ...) gets 1Password.  Hosts not in the table never reach `op',
;; so unrelated lookups don't trigger an unlock prompt.  The secret is read
;; only when the consumer asks for it, not when the entry is found.
;;
;; Set the table per machine in ~/.emacs.d/custom.el.  Each key is
;; (HOST [USER [PORT]]); an omitted or nil USER or PORT matches any.
;;
;;   (setq jps-op-references
;;         '((("localhost" "app")        . "op://Dev/app-db-dev/password")
;;           (("db.internal" "app" 5432) . "op://Prod/app-db/password")))
;;
;; Debug a lookup with (auth-source-search :host "localhost" :user "app").
;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'seq)

(defgroup jps-op nil
  "1Password secrets via the op CLI."
  :group 'auth-source)

(defcustom jps-op-executable "op"
  "The 1Password CLI executable."
  :type 'string)

(defcustom jps-op-references nil
  "Auth-source entries served from 1Password.
Each element is ((HOST [USER [PORT]]) . REFERENCE), where REFERENCE is an
op:// secret reference.  A nil or omitted USER or PORT matches any.
The first matching element wins."
  :type '(repeat (cons (list :tag "Match"
                             (string :tag "Host")
                             (choice :tag "User" (const nil) string)
                             (choice :tag "Port" (const nil) string integer))
                       (string :tag "op:// reference"))))

(defun jps-op-read (reference)
  "Return the secret that op:// REFERENCE points to.
Signal `user-error' when `op' is missing or the read fails."
  (unless (string-prefix-p "op://" reference)
    (user-error "Not a 1Password secret reference: %s" reference))
  (let ((op (or (executable-find jps-op-executable)
                (user-error "1Password CLI `%s' is not on exec-path"
                            jps-op-executable)))
        ;; `call-process' needs a local working directory.
        (default-directory (expand-file-name "~/"))
        (errfile (make-temp-file "jps-op-err")))
    (unwind-protect
        (with-temp-buffer
          (if (eq 0 (call-process op nil (list t errfile) nil
                                  "read" "--no-newline" reference))
              (buffer-string)
            (user-error "op read %s failed: %s" reference
                        (with-temp-buffer
                          (insert-file-contents errfile)
                          (string-trim (buffer-string))))))
      (delete-file errfile))))

(defun jps-op--field-matches-p (want got &optional required)
  "Return non-nil if table field WANT accepts auth-source search value GOT.
WANT nil matches anything.  GOT nil or t is a wildcard in the search,
accepted unless REQUIRED.  GOT may be a list of alternatives."
  (cond ((null want) t)
        ((memq got '(nil t)) (not required))
        (t (seq-some (lambda (g) (equal (format "%s" want) (format "%s" g)))
                     (ensure-list got)))))

(cl-defun jps-op--search (&key host user port max &allow-other-keys)
  "Auth-source search function over `jps-op-references'.
HOST is required; USER and PORT narrow the match.  Return at most MAX
entries whose :secret reads the reference when called."
  (let ((max (or max 1))
        found)
    (dolist (entry jps-op-references)
      ;; Fresh bindings per entry: each :secret closure keeps its own
      ;; reference.
      (seq-let (h u p) (car entry)
        (let ((reference (cdr entry)))
          (when (and (< (length found) max)
                     (jps-op--field-matches-p h host t)
                     (jps-op--field-matches-p u user)
                     (jps-op--field-matches-p p port))
            (push (list :host h
                        :user (or u (and (stringp user) user))
                        :port p
                        :secret (lambda () (jps-op-read reference)))
                  found)))))
    (nreverse found)))

(defvar jps-op--backend
  (auth-source-backend :source "1password"
                       :type 'jps-op
                       :search-function #'jps-op--search)
  "Auth-source backend serving `jps-op-references'.")

(defun jps-op--backend-parse (entry)
  "Return `jps-op--backend' when ENTRY in `auth-sources' is `jps-op'."
  (when (eq entry 'jps-op)
    (auth-source-backend-parse-parameters entry jps-op--backend)))

(add-hook 'auth-source-backend-parser-functions #'jps-op--backend-parse)
(add-to-list 'auth-sources 'jps-op)
(auth-source-forget-all-cached)

(provide 'jps-op)
;;; jps-op.el ends here
