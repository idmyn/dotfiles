;;; bits/evil.el -*- lexical-binding: t; -*-

(setq evil-disable-insert-state-bindings t)
(setq +evil-want-o/O-to-continue-comments nil)

(map!
 :i "C-d" 'delete-char
 :i "C-y" 'xah-paste-or-paste-previous)

(map!
 :map general-override-mode-map
 :n "RET" 'xah-open-file-at-cursor

 :m "H" 'evil-first-non-blank
 :m "L" 'evil-last-non-blank

 :mi "M-h" 'evil-window-left
 :mi "M-j" 'evil-window-down
 :mi "M-k" 'evil-window-up
 :mi "M-l" 'evil-window-right)

(map!
 :leader
 :prefix "f"
 :desc "Browse files" "d" 'dired-jump

 :prefix "t"
 :desc "Narrow" "n" 'md/narrow-dwim)

(map!
 :after dired
 :map dired-mode-map
 :n "h" (lambda () (interactive) (find-alternate-file ".."))
 :n "j" 'dired-next-line
 :n "k" 'dired-previous-line
 :n "l" 'dired-find-alternate-file

 :n "/" 'dired-narrow
 :n "C-g" 'revert-buffer

 :n "g" 'dired-git-info-mode
 :n "W" 'wdired-change-to-wdired-mode)
