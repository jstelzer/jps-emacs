;;; jps-operator.el --- Stoic Operator integration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Glue layer to hook stoic-operator.el into my config:
;; - enables stoic-operator-mode globally
;; - sets any machine-specific defaults

;;; Code:

(require 'stoic-operator)

;; If you want different defaults on mac vs abyss, you can branch here.
(setq stoic-operator-command "openai"
      stoic-operator-model "gpt-5.1")

(stoic-operator-mode 1)

(provide 'jps-operator)
;;; jps-operator.el ends here
