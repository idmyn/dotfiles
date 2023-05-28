;;; js.el -*- lexical-binding: t; -*-

(use-package! prettier
  :commands (prettier-prettify))

(setq prettier-mode-sync-config-flag nil)

(after! js2-mode
  (setq js2-basic-offset 2))

(after! typescript-mode
  (setq typescript-indent-level 2))

(add-hook! typescript-tsx-mode (setq-local prettier-parsers '(typescript)))

(after! web-mode
  (setq web-mode-code-indent-offset 2)
  (map!
   :map web-mode-map
   :i "M-/" 'dabbrev-expand))
