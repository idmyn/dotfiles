;;; bits/completion.el -*- lexical-binding: t; -*-

(after! orderless
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism orderless-flex)))

(setq consult-buffer-filter '("^ " "\\*.*\\*"))

(map!
 :leader
 :prefix "s"
 :desc "Search project" "p" 'deadgrep)

(after! deadgrep
  (setq deadgrep-display-buffer-function #'display-buffer)
  (set-popup-rule! "^\\*deadgrep.*\\*$" :size 0.4 :select t)
  (defun deadgrep--project-root ()
    "Guess the project root of the given FILE-PATH."
    (let ((root default-directory)
          (project (projectile-project-root)))
      (when project
        (setq root project))
      (when root
        (deadgrep--lookup-override root)))))

(use-package wgrep-deadgrep
  :load-path "../lisp/wgrep-deadgrep"
  :hook ((deadgrep-finished . wgrep-deadgrep-setup))
  :config (map!
           :map deadgrep-mode-map
           :n "i" 'wgrep-change-to-wgrep-mode))
