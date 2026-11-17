;;; jps-ellama.el --- Local LLMs via ellama + Ollama -*- lexical-binding: t -*-
;;; Commentary:
;; Ellama (pure elisp, on GNU ELPA's `llm' library) talking to a local Ollama.
;; Model choice depends on the machine, so each host picks a hardware profile:
;;
;;   mac-48     Apple Silicon, <60GB unified memory (M4 Max 48GB)
;;   mac-64     Apple Silicon, >=60GB unified memory (M5 64GB)
;;   cuda-32    Linux + NVIDIA with >=24GB VRAM (RTX 5090 32GB)
;;   small      anything else
;;
;; Detection runs when ellama first loads (C-c m), not at startup.  Override
;; per machine in ~/.emacs.d/custom.el:
;;
;;   (setq jps-ellama-profile 'mac-64)
;;
;; Pull the active profile's models with M-x jps-ellama-pull-models.
;; See README.md "Local LLMs (ellama + Ollama)" for the sizing rationale.
;;; Code:

(require 'use-package)
(require 'subr-x)

(declare-function make-llm-ollama "llm-ollama")

(defgroup jps-ellama nil
  "Local LLM settings for ellama."
  :group 'tools)

(defcustom jps-ellama-profile nil
  "Hardware profile used to choose Ollama models.
nil means detect it from the machine.  See `jps-ellama-profiles'."
  :type '(choice (const :tag "Auto-detect" nil) symbol))

(defcustom jps-ellama-ollama-host "localhost"
  "Host running the Ollama server."
  :type 'string)

(defcustom jps-ellama-agentic t
  "When non-nil, apply `ellama-setup-agentic-coding'.
That enables ellama's tools with DLP enforcement.  Without an SRT policy,
tool calls still ask for confirmation."
  :type 'boolean)

(defcustom jps-ellama-profiles
  '((mac-48
     ;; ~36GB GPU-wired by default.  One ~24GB model plus a 32k context.
     :coding "qwen3.6:35b-a3b-coding"   ; MoE, ~3B active: fast.  24GB
     :chat   "qwen3.8:27b"              ; dense Q4, agent-tuned.  18GB
     :num-ctx 32768)
    (mac-64
     ;; ~48GB GPU-wired by default.  Spend the headroom on Q8, not bigger models.
     :coding "qwen3.6:35b-a3b-q8_0"     ; 39GB
     :chat   "qwen3.8:27b-q8_0"         ; 30GB
     :num-ctx 65536)
    (cuda-32
     ;; 32GB VRAM.  Keep the whole model on the GPU; spilling to system
     ;; RAM costs far more speed than Q8 buys in quality.
     :coding "qwen3.6:35b-a3b-coding"   ; 24GB
     :chat   "qwen3.8:27b"              ; 18GB
     :num-ctx 32768)
    (small
     :coding "gemma4:12b"
     :chat   "gemma4:12b"
     :num-ctx 16384))
  "Ollama models and context size per hardware profile."
  :type '(alist :key-type symbol :value-type plist))

(defcustom jps-ellama-embedding-model "nomic-embed-text"
  "Ollama embedding model."
  :type 'string)

(defun jps-ellama--command-output (program &rest args)
  "Return trimmed stdout of PROGRAM with ARGS, or nil on failure."
  (when (executable-find program)
    (with-temp-buffer
      (when (eq 0 (apply #'call-process program nil '(t nil) nil args))
        (string-trim (buffer-string))))))

(defun jps-ellama--system-ram-gb ()
  "Return total system memory in GB, or nil if unknown."
  (pcase system-type
    ('darwin
     (when-let* ((bytes (jps-ellama--command-output "sysctl" "-n" "hw.memsize")))
       (/ (string-to-number bytes) (* 1024 1024 1024))))
    ('gnu/linux
     (with-temp-buffer
       (when (ignore-errors (insert-file-contents "/proc/meminfo") t)
         (when (re-search-forward "^MemTotal:\\s-+\\([0-9]+\\) kB" nil t)
           (/ (string-to-number (match-string 1)) (* 1024 1024))))))))

(defun jps-ellama--nvidia-vram-gb ()
  "Return the largest NVIDIA GPU's memory in GB, or nil."
  (when-let* ((out (jps-ellama--command-output
                    "nvidia-smi" "--query-gpu=memory.total"
                    "--format=csv,noheader,nounits")))
    (/ (apply #'max (mapcar #'string-to-number (split-string out "\n" t)))
       1024)))

(defun jps-ellama-detect-profile ()
  "Guess the hardware profile for this machine."
  (let ((ram (or (jps-ellama--system-ram-gb) 0)))
    (cond
     ((eq system-type 'darwin) (if (>= ram 60) 'mac-64 'mac-48))
     ((>= (or (jps-ellama--nvidia-vram-gb) 0) 24) 'cuda-32)
     (t 'small))))

(defun jps-ellama-active-profile ()
  "Return the profile in use: `jps-ellama-profile' or the detected one."
  (or jps-ellama-profile (jps-ellama-detect-profile)))

(defun jps-ellama--spec ()
  "Return the plist for the active profile."
  (let ((profile (jps-ellama-active-profile)))
    (or (alist-get profile jps-ellama-profiles)
        (user-error "No entry for profile `%s' in jps-ellama-profiles" profile))))

(defun jps-ellama--provider (model num-ctx)
  "Return an Ollama provider for MODEL with context NUM-CTX."
  (make-llm-ollama
   :host jps-ellama-ollama-host
   :chat-model model
   :embedding-model jps-ellama-embedding-model
   :default-chat-non-standard-params `(("num_ctx" . ,num-ctx))))

(defun jps-ellama-provider (&optional slot model)
  "Return an Ollama provider for SLOT (`:chat' or `:coding') of the profile.
SLOT defaults to `:chat'.  MODEL, when non-nil, replaces the profile's
model but keeps its context size.  Other modules (Stoic Operator) use this
so every local model goes through one place."
  (require 'llm-ollama)
  (let ((spec (jps-ellama--spec)))
    (jps-ellama--provider (or model (plist-get spec (or slot :chat)))
                          (plist-get spec :num-ctx))))

(defun jps-ellama-apply-profile ()
  "Point ellama's providers at the active profile's models."
  (interactive)
  (require 'llm-ollama)
  (let* ((spec (jps-ellama--spec))
         (ctx (plist-get spec :num-ctx))
         (coding (jps-ellama--provider (plist-get spec :coding) ctx))
         (chat (jps-ellama--provider (plist-get spec :chat) ctx)))
    (setopt ellama-provider chat
            ellama-coding-provider coding
            ;; The MoE model answers fastest; use it for the small jobs.
            ellama-naming-provider coding
            ellama-summarization-provider coding
            ellama-translation-provider chat
            ellama-extraction-provider coding
            ellama-providers `((,(plist-get spec :chat) . ,chat)
                               (,(plist-get spec :coding) . ,coding)))
    (message "ellama: profile %s (chat %s, coding %s, ctx %d)"
             (jps-ellama-active-profile)
             (plist-get spec :chat) (plist-get spec :coding) ctx)))

(defun jps-ellama-pull-models ()
  "Pull the active profile's Ollama models in a compilation buffer."
  (interactive)
  (unless (executable-find "ollama")
    (user-error "Ollama is not on exec-path"))
  (let* ((spec (jps-ellama--spec))
         (models (delete-dups (list (plist-get spec :chat)
                                    (plist-get spec :coding)
                                    jps-ellama-embedding-model))))
    (compile (mapconcat (lambda (m) (concat "ollama pull " (shell-quote-argument m)))
                        models " && "))))

(use-package ellama
  :straight t
  :bind ("C-c m" . ellama)
  :hook (org-ctrl-c-ctrl-c-hook . ellama-chat-send-last-message)
  :config
  (jps-ellama-apply-profile)
  (when jps-ellama-agentic
    (ellama-setup-agentic-coding))
  (setopt ellama-auto-scroll t)
  (ellama-context-header-line-global-mode +1)
  (ellama-session-header-line-global-mode +1))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c m" "ellama (local LLM)"))

(provide 'jps-ellama)
;;; jps-ellama.el ends here
