;;; bits/completion.el -*- lexical-binding: t; -*-

(use-package corfu
  :config
  (setq corfu-auto t
         corfu-quit-no-match 'separator) ;; or t
  (map! :map global-map
        "C-SPC" #'completion-at-point)
  (map! :map corfu-map
        "C-j" #'corfu-next
        "C-k" #'corfu-previous)
  :init
  (corfu-global-mode +1))
