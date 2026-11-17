;;; stoic-operator.el --- Stoic Operator Forge  -*- lexical-binding: t; -*-
;;; Commentary:
;; Personal writing assistant for Field Manual and register work.
;;
;; Local models only: this content is kept off cloud providers on purpose,
;; so the provider must be Ollama.  By default it uses the chat model of the
;; jps-ellama hardware profile (see personal/jps-ellama.el); set
;; `stoic-operator-model' to use a different Ollama model.
;;
;; Responses stream into *Stoic-Operator* without blocking Emacs.  The
;; optional system contract in `stoic-operator-system-contract-file' is sent
;; as the system prompt.
;;
;; Keys: C-c o m (menu), C-c o d (doctrine), C-c o o (outline),
;; C-c o t (translate register).
;;; Code:

(require 'transient)
(require 'jps-ellama)

;; llm is loaded on first use (via `jps-ellama-provider'), not at startup.
(declare-function llm-make-chat-prompt "llm")
(declare-function llm-chat-streaming "llm")
(declare-function llm-ollama-p "llm-ollama")
(declare-function llm-ollama-chat-model "llm-ollama")

(defgroup stoic-operator nil
  "Personal cognitive assistant for Stoic Operator work."
  :group 'tools)

(defcustom stoic-operator-model nil
  "Ollama model for Stoic Operator.
nil means the chat model of the active jps-ellama profile."
  :type '(choice (const :tag "Follow jps-ellama profile" nil) string))

(defcustom stoic-operator-system-contract-file
  (expand-file-name "~/.stoic-operator-system.txt")
  "File containing the system contract (voice and rules for every command)."
  :type 'file)

(defcustom stoic-operator-registers
  '("operator-technical" "partner-friendly" "therapist-brief" "public-readable")
  "Communication registers offered by `stoic-operator-translate-register'."
  :type '(repeat string))

(defconst stoic-operator--buffer "*Stoic-Operator*")

(defun stoic-operator--system-contract ()
  "Return the system contract text, or nil."
  (when (file-readable-p stoic-operator-system-contract-file)
    (with-temp-buffer
      (insert-file-contents stoic-operator-system-contract-file)
      (buffer-string))))

(defun stoic-operator--provider ()
  "Return the Ollama provider, refusing anything that is not local."
  (let ((provider (jps-ellama-provider :chat stoic-operator-model)))
    (unless (llm-ollama-p provider)
      (user-error "Stoic Operator only runs on local Ollama models"))
    provider))

(defun stoic-operator--call (task text)
  "Send TASK applied to TEXT and stream the reply into the output buffer."
  (let* ((provider (stoic-operator--provider))
         (prompt (llm-make-chat-prompt
                  (format "%s\n\n----- INPUT -----\n\n%s" task text)
                  :context (stoic-operator--system-contract)))
         (buf (get-buffer-create stoic-operator--buffer))
         (show (lambda (string)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       (insert string)))))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "[%s is thinking...]" (llm-ollama-chat-model provider))))
      (unless (derived-mode-p 'markdown-mode 'text-mode)
        (if (fboundp 'markdown-mode) (markdown-mode) (text-mode)))
      (visual-line-mode 1))
    (display-buffer buf)
    (llm-chat-streaming
     provider prompt
     show
     (lambda (final)
       (funcall show final)
       (message "Stoic Operator: done"))
     (lambda (_type msg)
       (funcall show (format "Stoic Operator error: %s" msg))))))

(defun stoic-operator--region-text (beg end)
  "Return the text between BEG and END, or signal when there is none."
  (unless (and beg end (< beg end))
    (user-error "Select a region first"))
  (buffer-substring-no-properties beg end))

;;;###autoload
(defun stoic-operator-refine-region-as-doctrine (beg end)
  "Rewrite the region from BEG to END as Field Manual doctrine."
  (interactive "r")
  (stoic-operator--call
   "Rewrite this as a section of the Stoic Operator's Field Manual.
Keep it structural, recursive, non-clinical. Use clear headings if helpful."
   (stoic-operator--region-text beg end)))

;;;###autoload
(defun stoic-operator-outline-chapter (beg end)
  "Propose a chapter outline from the notes between BEG and END."
  (interactive "r")
  (stoic-operator--call
   "From these notes, propose a chapter outline (sections + 1-2 sentence \
descriptions) for the Stoic Operator's Field Manual."
   (stoic-operator--region-text beg end)))

;;;###autoload
(defun stoic-operator-translate-register (beg end target)
  "Rewrite the region from BEG to END into the TARGET register."
  (interactive
   (list (region-beginning) (region-end)
         (completing-read "Target register: " stoic-operator-registers nil t)))
  (stoic-operator--call
   (format "Rewrite this into the %s register while preserving meaning.
No diagnosis, no therapy talk. Be concise and clear." target)
   (stoic-operator--region-text beg end)))

(transient-define-prefix stoic-operator-transient ()
  "Stoic Operator commands."
  ["Stoic Operator"
   ["Doctrine"
    ("d" "Refine region as doctrine" stoic-operator-refine-region-as-doctrine)
    ("o" "Outline chapter from region" stoic-operator-outline-chapter)]
   ["Translate"
    ("t" "Translate register" stoic-operator-translate-register)]])

(defvar-keymap stoic-operator-command-map
  :doc "Stoic Operator commands, under C-c o."
  "m" #'stoic-operator-transient
  "d" #'stoic-operator-refine-region-as-doctrine
  "o" #'stoic-operator-outline-chapter
  "t" #'stoic-operator-translate-register)

(keymap-global-set "C-c o" stoic-operator-command-map)

(provide 'stoic-operator)
;;; stoic-operator.el ends here
