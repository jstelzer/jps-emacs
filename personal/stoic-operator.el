;;; stoic-operator.el --- Stoic Operator Forge  -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'transient)

(defgroup stoic-operator nil
  "Personal cognitive assistant for Stoic Operator work."
  :group 'tools)

(defcustom stoic-operator-backend 'ollama
  "Backend to use for Stoic Operator.
Possible values: 'openai or 'ollama."
  :type '(choice (const :tag "OpenAI" openai)
                 (const :tag "Ollama" ollama))
  :group 'stoic-operator)

(defcustom stoic-operator-command "openai"
  "Shell command that talks to OpenAI (or other LLM)."
  :type 'string
  :group 'stoic-operator)

(defcustom stoic-operator-model "gpt-4o"
  "Model name/flag passed to `stoic-operator-command` (for OpenAI backend)."
  :type 'string
  :group 'stoic-operator)

(defcustom stoic-operator-ollama-model "deepseek-r1:14b"
  "Ollama model name to run for Stoic Operator."
  :type 'string
  :group 'stoic-operator)

(defcustom stoic-operator-system-contract-file
  (expand-file-name "~/.stoic-operator-system.txt")
  "File containing the SYSTEM contract / voice seal."
  :type 'file)

(defun stoic-operator--system-contract ()
  "Read the system contract text."
  (when (file-readable-p stoic-operator-system-contract-file)
    (with-temp-buffer
      (insert-file-contents stoic-operator-system-contract-file)
      (buffer-string))))

(defun stoic-operator--call (task text)
  "Call the configured backend (OpenAI or Ollama) and show result in *Stoic-Operator*."
  (let* ((sys  (or (stoic-operator--system-contract) ""))
         (user (format "%s\n\n----- INPUT -----\n\n%s" task text))
         (buf  (get-buffer-create "*Stoic-Operator*")))
    (with-current-buffer buf
      (erase-buffer)
      (pcase stoic-operator-backend
        ('openai
         (call-process stoic-operator-command
                       nil buf nil
                       "api" "chat.completions.create"
                       "-m" stoic-operator-model
                       "-g" "system" sys
                       "-g" "user" user))
        ('ollama
         (let* ((full (format "SYSTEM:\n%s\n\nTASK:\n%s\n\n%s" sys task user))
                (process-environment
                 (cons "TERM=dumb" process-environment)))
           (with-temp-buffer
             (insert full)
             (call-process-region (point-min) (point-max)
                                  "ollama" nil buf nil
                                  "run" stoic-operator-ollama-model))))
        (_
         (insert (format "Unknown backend: %S" stoic-operator-backend))))

      ;; Strip/handle ANSI: this removes escape codes and applies faces
      (ansi-color-apply-on-region (point-min) (point-max))

      (goto-char (point-min)))
    (pop-to-buffer buf)))

;;;###autoload
(defun stoic-operator-refine-region-as-doctrine (beg end)
  "Turn region into Stoic Operator field-manual style doctrine."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (task "Rewrite this as a section of the Stoic Operator’s Field Manual.
Keep it structural, recursive, non-clinical. Use clear headings if helpful."))
    (stoic-operator--call task text)))

;;;###autoload
(defun stoic-operator-outline-chapter (beg end)
  "Propose a chapter outline from region notes."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (task "From these notes, propose a chapter outline (sections + 1–2 sentence descriptions) for the Stoic Operator’s Field Manual."))
    (stoic-operator--call task text)))

;;;###autoload
(defun stoic-operator-translate-register (beg end target)
  "Translate region into TARGET communication register."
  (interactive
   (list (region-beginning) (region-end)
         (completing-read "Target register: "
                          '("operator-technical"
                            "partner-friendly"
                            "therapist-brief"
                            "public-readable"))))
  (let* ((text (buffer-substring-no-properties beg end))
         (task (format "Rewrite this into the %s register while preserving meaning.
No diagnosis, no therapy talk. Be concise and clear."
                       target)))
    (stoic-operator--call task text)))

;; Transient menu, inspired by claude-code
(transient-define-prefix stoic-operator-transient ()
  "Stoic Operator commands."
  ["Stoic Operator"
   ["Doctrine"
    ("d" "Refine region as doctrine" stoic-operator-refine-region-as-doctrine)
    ("o" "Outline chapter from region" stoic-operator-outline-chapter)]
   ["Translate"
    ("t" "Translate register" stoic-operator-translate-register)]])

(defvar stoic-operator-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") #'stoic-operator-transient)
    (define-key map (kbd "d") #'stoic-operator-refine-region-as-doctrine)
    (define-key map (kbd "o") #'stoic-operator-outline-chapter)
    (define-key map (kbd "t") #'stoic-operator-translate-register)
    map)
  "Keymap for Stoic Operator commands under C-c o.")

;;;###autoload
(define-minor-mode stoic-operator-mode
  "Minor mode for Stoic Operator integration."
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            ;; C-c o as the Operator prefix (README says it's free)
            (define-key map (kbd "C-c o") stoic-operator-command-map)
            map))

(provide 'stoic-operator)
;;; stoic-operator.el ends here
