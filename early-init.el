;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Optimize startup by preventing package.el from loading
(setq package-enable-at-startup nil)

;; Prevent unwanted runtime compilation for gccemacs (native compilation) users
(setq native-comp-deferred-compilation nil)

;; Prefer loading .el files over .elc files for easier debugging during development
(setq load-prefer-newer t)

;; UI optimizations
(when (display-graphic-p)
  ;; Disable GUI elements early to prevent flashing
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  
  ;; Reduce startup flash by setting frame parameters early
  (setq default-frame-alist
        '((vertical-scroll-bars . nil)
          (horizontal-scroll-bars . nil)
          (menu-bar-lines . 0)
          (tool-bar-lines . 0))))

;;; early-init.el ends here