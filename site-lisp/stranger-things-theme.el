;;; stranger-things-theme.el --- A theme inspired by Stranger Things -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Author: Original by doom-themes contributors, vendored without doom dependencies
;; Version: 1.0.0
;; Keywords: faces, theme
;; Package-Requires: ((emacs "24.1"))
;;
;;; Commentary:
;;
;; A dark, moody theme inspired by the Netflix series Stranger Things.
;; Features the iconic red title aesthetic, dark blues and blacks reminiscent
;; of the Upside Down, and neon 80s colors. Think vintage synth-wave meets
;; supernatural horror.
;;
;; This is a standalone version that doesn't require doom-themes.
;;
;;; Code:

(deftheme stranger-things
  "A dark, atmospheric theme inspired by Stranger Things with neon 80s vibes.")

(defgroup stranger-things-theme nil
  "Options for stranger-things theme."
  :group 'faces)

(defcustom stranger-things-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'stranger-things-theme
  :type 'boolean)

(defcustom stranger-things-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'stranger-things-theme
  :type 'boolean)

(defcustom stranger-things-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to specify exact padding."
  :group 'stranger-things-theme
  :type '(choice integer boolean))

;;; Color palette
(let* ((class '((class color) (min-colors 89)))
       ;; Base colors
       (bg         "#000000")
       (bg-alt     "#0d0d15")
       (base0      "#121218")
       (base1      "#161622")
       (base2      "#1a1a2e")
       (base3      "#242438")
       (base4      "#2e2e45")
       (base5      "#3d3d5c")
       (base6      "#52526e")
       (base8      "#9999b3")
       (fg-alt     "#b3b3cc")
       (fg         "#e6e6f2")

       ;; Accent colors
       (grey       "#5a5a75")
       (red        "#ff0000")
       (orange     "#ff6b35")
       (green      "#39ff14")
       (teal       "#00ffcc")
       (yellow     "#ffed4e")
       (blue       "#00b4d8")
       (magenta    "#ff006e")
       (violet     "#bb00ff")
       (cyan       "#00ffff")
       (dark-cyan  "#0096c7")

       ;; Derived colors
       (comments   (if stranger-things-brighter-comments dark-cyan grey))
       (modeline-bg (if stranger-things-brighter-modeline base3 "#000000"))
       (modeline-bg-inactive bg)
       (modeline-pad (when stranger-things-padded-modeline
                       (if (integerp stranger-things-padded-modeline)
                           stranger-things-padded-modeline 4))))

  (custom-theme-set-faces
   'stranger-things

   ;; Basic faces
   `(default ((,class (:foreground ,fg :background ,bg))))
   `(cursor ((,class (:background ,red))))
   `(fringe ((,class (:background ,bg-alt))))
   `(hl-line ((,class (:background ,base2))))
   `(region ((,class (:background ,base3 :extend t))))
   `(secondary-selection ((,class (:background ,base4))))
   `(minibuffer-prompt ((,class (:foreground ,cyan :weight bold))))
   `(vertical-border ((,class (:foreground ,base1))))
   `(window-divider ((,class (:foreground ,base1))))
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foreground ,violet :underline t))))
   `(shadow ((,class (:foreground ,base5))))
   `(error ((,class (:foreground ,red))))
   `(warning ((,class (:foreground ,yellow))))
   `(success ((,class (:foreground ,green))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,base5 :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,red :background ,bg :weight bold))))

   ;; Font-lock faces
   `(font-lock-builtin-face ((,class (:foreground ,cyan))))
   `(font-lock-comment-face ((,class (:foreground ,comments :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comments))))
   `(font-lock-doc-face ((,class (:foreground ,teal))))
   `(font-lock-constant-face ((,class (:foreground ,violet))))
   `(font-lock-function-name-face ((,class (:foreground ,blue))))
   `(font-lock-keyword-face ((,class (:foreground ,red))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,yellow))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg-alt))))
   `(font-lock-warning-face ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-negation-char-face ((,class (:foreground ,magenta))))
   `(font-lock-preprocessor-face ((,class (:foreground ,magenta))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,magenta))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,magenta))))

   ;; Mode-line
   `(mode-line ((,class (:foreground ,fg :background ,modeline-bg
                         ,@(when modeline-pad `(:box (:line-width ,modeline-pad :color ,modeline-bg)))))))
   `(mode-line-inactive ((,class (:foreground ,base6 :background ,modeline-bg-inactive
                                  ,@(when modeline-pad `(:box (:line-width ,modeline-pad :color ,modeline-bg-inactive)))))))
   `(mode-line-emphasis ((,class (:foreground ,(if stranger-things-brighter-modeline base8 red)))))
   `(mode-line-buffer-id ((,class (:foreground ,cyan :weight bold))))
   `(mode-line-highlight ((,class (:foreground ,red))))

   ;; Header-line
   `(header-line ((,class (:background ,bg-alt :foreground ,fg))))

   ;; Isearch
   `(isearch ((,class (:foreground ,bg :background ,red :weight bold))))
   `(isearch-fail ((,class (:foreground ,red :background ,bg))))
   `(lazy-highlight ((,class (:foreground ,fg :background ,base3 :weight bold))))

   ;; Highlight
   `(highlight ((,class (:background ,base3 :foreground ,fg))))
   `(match ((,class (:foreground ,red :weight bold))))

   ;; Whitespace
   `(whitespace-empty ((,class (:background ,base2))))
   `(whitespace-hspace ((,class (:foreground ,base4))))
   `(whitespace-indentation ((,class (:foreground ,base4))))
   `(whitespace-line ((,class (:background ,base2))))
   `(whitespace-newline ((,class (:foreground ,base4))))
   `(whitespace-space ((,class (:foreground ,base4))))
   `(whitespace-tab ((,class (:foreground ,base4))))
   `(whitespace-trailing ((,class (:background ,red))))

   ;; Parenthesis matching
   `(show-paren-match ((,class (:foreground ,red :background ,base3 :weight bold))))
   `(show-paren-mismatch ((,class (:foreground ,bg :background ,red :weight bold))))

   ;; Dired
   `(dired-directory ((,class (:foreground ,cyan :weight bold))))
   `(dired-flagged ((,class (:foreground ,red))))
   `(dired-header ((,class (:foreground ,magenta :weight bold))))
   `(dired-ignored ((,class (:foreground ,grey))))
   `(dired-mark ((,class (:foreground ,red :weight bold))))
   `(dired-marked ((,class (:foreground ,magenta :weight bold))))
   `(dired-perm-write ((,class (:foreground ,fg))))
   `(dired-symlink ((,class (:foreground ,violet))))
   `(dired-warning ((,class (:foreground ,yellow))))

   ;; Completions
   `(completions-annotations ((,class (:foreground ,grey))))
   `(completions-common-part ((,class (:foreground ,red))))
   `(completions-first-difference ((,class (:foreground ,cyan :weight bold))))

   ;; Company
   `(company-tooltip ((,class (:background ,base2 :foreground ,fg))))
   `(company-tooltip-common ((,class (:foreground ,red))))
   `(company-tooltip-selection ((,class (:background ,base4 :foreground ,red))))
   `(company-tooltip-annotation ((,class (:foreground ,grey))))
   `(company-scrollbar-bg ((,class (:background ,base2))))
   `(company-scrollbar-fg ((,class (:background ,base5))))
   `(company-preview ((,class (:foreground ,grey))))
   `(company-preview-common ((,class (:foreground ,red :background ,base2))))

   ;; Vertico
   `(vertico-current ((,class (:background ,base3 :foreground ,red :weight bold))))

   ;; Orderless
   `(orderless-match-face-0 ((,class (:foreground ,red :weight bold))))
   `(orderless-match-face-1 ((,class (:foreground ,cyan :weight bold))))
   `(orderless-match-face-2 ((,class (:foreground ,magenta :weight bold))))
   `(orderless-match-face-3 ((,class (:foreground ,green :weight bold))))

   ;; Marginalia
   `(marginalia-documentation ((,class (:foreground ,grey :slant italic))))
   `(marginalia-file-priv-dir ((,class (:foreground ,cyan))))
   `(marginalia-file-priv-exec ((,class (:foreground ,green))))
   `(marginalia-file-priv-link ((,class (:foreground ,violet))))
   `(marginalia-file-priv-read ((,class (:foreground ,yellow))))
   `(marginalia-file-priv-write ((,class (:foreground ,red))))
   `(marginalia-key ((,class (:foreground ,cyan))))
   `(marginalia-mode ((,class (:foreground ,magenta))))
   `(marginalia-number ((,class (:foreground ,orange))))

   ;; Consult
   `(consult-file ((,class (:foreground ,fg))))
   `(consult-bookmark ((,class (:foreground ,violet))))
   `(consult-buffer ((,class (:foreground ,cyan))))
   `(consult-line-number ((,class (:foreground ,base5))))
   `(consult-preview-match ((,class (:background ,base3 :foreground ,red))))

   ;; Magit
   `(magit-blame-heading ((,class (:foreground ,orange :background ,base3))))
   `(magit-bisect-bad ((,class (:foreground ,red))))
   `(magit-bisect-good ((,class (:foreground ,green))))
   `(magit-bisect-skip ((,class (:foreground ,orange))))
   `(magit-branch-local ((,class (:foreground ,cyan))))
   `(magit-branch-remote ((,class (:foreground ,green))))
   `(magit-diff-added ((,class (:foreground ,green :background "#0a2a0a" :extend t))))
   `(magit-diff-added-highlight ((,class (:foreground ,green :background "#0f3f0f" :weight bold :extend t))))
   `(magit-diff-removed ((,class (:foreground "#cc0000" :background "#2a0a0a" :extend t))))
   `(magit-diff-removed-highlight ((,class (:foreground ,red :background "#3f0f0f" :weight bold :extend t))))
   `(magit-diff-base ((,class (:foreground ,orange :background "#2a2a0a" :extend t))))
   `(magit-diff-base-highlight ((,class (:foreground ,orange :background "#3f3f0f" :weight bold :extend t))))
   `(magit-diff-context ((,class (:foreground ,grey :extend t))))
   `(magit-diff-context-highlight ((,class (:foreground ,fg-alt :background ,base1 :extend t))))
   `(magit-diff-file-heading ((,class (:foreground ,fg :weight bold))))
   `(magit-diff-file-heading-highlight ((,class (:foreground ,fg :background ,base3 :weight bold))))
   `(magit-diff-file-heading-selection ((,class (:foreground ,red :background ,base3))))
   `(magit-diff-hunk-heading ((,class (:foreground ,fg :background ,base3))))
   `(magit-diff-hunk-heading-highlight ((,class (:foreground ,fg :background ,base4))))
   `(magit-diffstat-added ((,class (:foreground ,green))))
   `(magit-diffstat-removed ((,class (:foreground ,red))))
   `(magit-hash ((,class (:foreground ,grey))))
   `(magit-header-line ((,class (:foreground ,fg :weight bold))))
   `(magit-log-author ((,class (:foreground ,orange))))
   `(magit-log-date ((,class (:foreground ,blue))))
   `(magit-log-graph ((,class (:foreground ,grey))))
   `(magit-process-ng ((,class (:foreground ,red))))
   `(magit-process-ok ((,class (:foreground ,green))))
   `(magit-reflog-amend ((,class (:foreground ,magenta))))
   `(magit-reflog-checkout ((,class (:foreground ,blue))))
   `(magit-reflog-cherry-pick ((,class (:foreground ,green))))
   `(magit-reflog-commit ((,class (:foreground ,green))))
   `(magit-reflog-merge ((,class (:foreground ,green))))
   `(magit-reflog-other ((,class (:foreground ,cyan))))
   `(magit-reflog-rebase ((,class (:foreground ,magenta))))
   `(magit-reflog-remote ((,class (:foreground ,cyan))))
   `(magit-reflog-reset ((,class (:foreground ,red))))
   `(magit-section-heading ((,class (:foreground ,cyan :weight bold))))
   `(magit-section-heading-selection ((,class (:foreground ,red :extend t))))
   `(magit-section-highlight ((,class (:background ,base2 :extend t))))
   `(magit-sequence-drop ((,class (:foreground ,red))))
   `(magit-sequence-head ((,class (:foreground ,blue))))
   `(magit-sequence-part ((,class (:foreground ,orange))))
   `(magit-sequence-stop ((,class (:foreground ,green))))
   `(magit-signature-bad ((,class (:foreground ,red :weight bold))))
   `(magit-signature-error ((,class (:foreground ,red))))
   `(magit-signature-expired ((,class (:foreground ,orange))))
   `(magit-signature-good ((,class (:foreground ,green))))
   `(magit-signature-revoked ((,class (:foreground ,magenta))))
   `(magit-signature-untrusted ((,class (:foreground ,yellow))))
   `(magit-tag ((,class (:foreground ,yellow))))

   ;; Git-gutter
   `(git-gutter:added ((,class (:foreground ,green :weight bold))))
   `(git-gutter:deleted ((,class (:foreground ,red :weight bold))))
   `(git-gutter:modified ((,class (:foreground ,orange :weight bold))))

   ;; Markdown
   `(markdown-markup-face ((,class (:foreground ,base5))))
   `(markdown-header-face ((,class (:inherit bold :foreground ,red))))
   `(markdown-header-face-1 ((,class (:inherit bold :foreground ,red :height 1.3))))
   `(markdown-header-face-2 ((,class (:inherit bold :foreground ,magenta :height 1.2))))
   `(markdown-header-face-3 ((,class (:inherit bold :foreground ,violet :height 1.1))))
   `(markdown-header-face-4 ((,class (:inherit bold :foreground ,cyan))))
   `(markdown-header-face-5 ((,class (:foreground ,blue))))
   `(markdown-header-face-6 ((,class (:foreground ,teal))))
   `(markdown-code-face ((,class (:background ,base2))))
   `(markdown-inline-code-face ((,class (:foreground ,green :background ,base2))))
   `(markdown-bold-face ((,class (:foreground ,orange :weight bold))))
   `(markdown-italic-face ((,class (:foreground ,violet :slant italic))))
   `(markdown-link-face ((,class (:foreground ,blue))))
   `(markdown-url-face ((,class (:foreground ,cyan :underline t))))

   ;; Org-mode
   `(org-block ((,class (:background ,base0 :extend t))))
   `(org-block-begin-line ((,class (:foreground ,grey :background ,base0 :extend t))))
   `(org-block-end-line ((,class (:foreground ,grey :background ,base0 :extend t))))
   `(org-code ((,class (:foreground ,cyan))))
   `(org-document-info-keyword ((,class (:foreground ,grey))))
   `(org-document-info ((,class (:foreground ,cyan))))
   `(org-document-title ((,class (:foreground ,red :weight bold :height 1.4))))
   `(org-level-1 ((,class (:foreground ,red :weight bold :height 1.3))))
   `(org-level-2 ((,class (:foreground ,magenta :weight bold :height 1.2))))
   `(org-level-3 ((,class (:foreground ,violet :weight bold :height 1.1))))
   `(org-level-4 ((,class (:foreground ,cyan :weight bold))))
   `(org-level-5 ((,class (:foreground ,blue))))
   `(org-level-6 ((,class (:foreground ,teal))))
   `(org-level-7 ((,class (:foreground ,green))))
   `(org-level-8 ((,class (:foreground ,yellow))))
   `(org-link ((,class (:foreground ,blue :underline t))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-headline-done ((,class (:foreground ,grey))))
   `(org-checkbox ((,class (:foreground ,cyan))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,cyan))))
   `(org-checkbox-statistics-done ((,class (:foreground ,green))))
   `(org-date ((,class (:foreground ,teal :underline t))))
   `(org-drawer ((,class (:foreground ,grey))))
   `(org-ellipsis ((,class (:foreground ,grey))))
   `(org-footnote ((,class (:foreground ,blue))))
   `(org-formula ((,class (:foreground ,orange))))
   `(org-hide ((,class (:foreground ,bg))))
   `(org-meta-line ((,class (:foreground ,grey))))
   `(org-priority ((,class (:foreground ,red))))
   `(org-property-value ((,class (:foreground ,fg-alt))))
   `(org-quote ((,class (:background ,base2 :slant italic :extend t))))
   `(org-scheduled ((,class (:foreground ,green))))
   `(org-scheduled-previously ((,class (:foreground ,yellow))))
   `(org-scheduled-today ((,class (:foreground ,green))))
   `(org-sexp-date ((,class (:foreground ,teal))))
   `(org-special-keyword ((,class (:foreground ,grey))))
   `(org-table ((,class (:foreground ,cyan))))
   `(org-tag ((,class (:foreground ,grey :weight normal))))
   `(org-upcoming-deadline ((,class (:foreground ,orange))))
   `(org-verbatim ((,class (:foreground ,green))))
   `(org-verse ((,class (:background ,base2 :slant italic :extend t))))
   `(org-warning ((,class (:foreground ,yellow :weight bold))))

   ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,red))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,cyan))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,magenta))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,violet))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,teal))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red :weight bold))))

   ;; Treemacs
   `(treemacs-root-face ((,class (:foreground ,red :weight bold :height 1.2))))
   `(treemacs-directory-face ((,class (:foreground ,cyan))))
   `(treemacs-file-face ((,class (:foreground ,fg))))
   `(treemacs-git-modified-face ((,class (:foreground ,orange))))
   `(treemacs-git-added-face ((,class (:foreground ,green))))
   `(treemacs-git-conflict-face ((,class (:foreground ,red))))
   `(treemacs-git-untracked-face ((,class (:foreground ,grey))))

   ;; Which-key
   `(which-key-key-face ((,class (:foreground ,red))))
   `(which-key-group-description-face ((,class (:foreground ,cyan))))
   `(which-key-command-description-face ((,class (:foreground ,fg))))
   `(which-key-separator-face ((,class (:foreground ,grey))))

   ;; Flycheck
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,yellow)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,cyan)))))
   `(flycheck-fringe-error ((,class (:foreground ,red))))
   `(flycheck-fringe-warning ((,class (:foreground ,yellow))))
   `(flycheck-fringe-info ((,class (:foreground ,cyan))))

   ;; Flymake
   `(flymake-error ((,class (:underline (:style wave :color ,red)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,yellow)))))
   `(flymake-note ((,class (:underline (:style wave :color ,cyan)))))

   ;; Eglot
   `(eglot-highlight-symbol-face ((,class (:background ,base3 :weight bold))))

   ;; Vterm
   `(vterm-color-black ((,class (:foreground ,base0 :background ,base4))))
   `(vterm-color-red ((,class (:foreground ,red :background ,red))))
   `(vterm-color-green ((,class (:foreground ,green :background ,green))))
   `(vterm-color-yellow ((,class (:foreground ,yellow :background ,yellow))))
   `(vterm-color-blue ((,class (:foreground ,blue :background ,blue))))
   `(vterm-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
   `(vterm-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
   `(vterm-color-white ((,class (:foreground ,fg :background ,fg))))

   ;; Eshell
   `(eshell-prompt ((,class (:foreground ,red :weight bold))))
   `(eshell-ls-archive ((,class (:foreground ,magenta))))
   `(eshell-ls-backup ((,class (:foreground ,grey))))
   `(eshell-ls-clutter ((,class (:foreground ,grey))))
   `(eshell-ls-directory ((,class (:foreground ,cyan :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,green))))
   `(eshell-ls-missing ((,class (:foreground ,red))))
   `(eshell-ls-product ((,class (:foreground ,orange))))
   `(eshell-ls-readonly ((,class (:foreground ,violet))))
   `(eshell-ls-special ((,class (:foreground ,magenta))))
   `(eshell-ls-symlink ((,class (:foreground ,violet))))
   `(eshell-ls-unreadable ((,class (:foreground ,grey))))

   ;; Tab-bar
   `(tab-bar ((,class (:background ,bg-alt :foreground ,fg))))
   `(tab-bar-tab ((,class (:background ,bg :foreground ,red :weight bold))))
   `(tab-bar-tab-inactive ((,class (:background ,bg-alt :foreground ,grey))))

   ;; Compilation
   `(compilation-error ((,class (:foreground ,red :weight bold))))
   `(compilation-warning ((,class (:foreground ,yellow))))
   `(compilation-info ((,class (:foreground ,cyan))))
   `(compilation-mode-line-fail ((,class (:foreground ,red :weight bold))))
   `(compilation-mode-line-exit ((,class (:foreground ,green :weight bold))))

   ;; Avy
   `(avy-lead-face ((,class (:background ,red :foreground ,bg :weight bold))))
   `(avy-lead-face-0 ((,class (:background ,cyan :foreground ,bg :weight bold))))
   `(avy-lead-face-1 ((,class (:background ,magenta :foreground ,bg :weight bold))))
   `(avy-lead-face-2 ((,class (:background ,green :foreground ,bg :weight bold))))

   ;; Transient
   `(transient-key ((,class (:foreground ,red :weight bold))))
   `(transient-argument ((,class (:foreground ,green))))
   `(transient-value ((,class (:foreground ,cyan))))
   `(transient-inactive-argument ((,class (:foreground ,grey))))
   `(transient-inactive-value ((,class (:foreground ,grey))))
   `(transient-heading ((,class (:foreground ,cyan :weight bold))))
   `(transient-active-infix ((,class (:background ,base3))))

   ;; Info
   `(info-menu-star ((,class (:foreground ,red))))
   `(info-node ((,class (:foreground ,red :weight bold))))
   `(info-title-1 ((,class (:foreground ,red :weight bold :height 1.3))))
   `(info-title-2 ((,class (:foreground ,magenta :weight bold :height 1.2))))
   `(info-title-3 ((,class (:foreground ,violet :weight bold :height 1.1))))
   `(info-title-4 ((,class (:foreground ,cyan :weight bold))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;;###autoload
(defun stranger-things-theme-reload ()
  "Reload the stranger-things theme to apply customization changes."
  (interactive)
  (disable-theme 'stranger-things)
  (load-theme 'stranger-things t))

(provide-theme 'stranger-things)

;;; stranger-things-theme.el ends here
