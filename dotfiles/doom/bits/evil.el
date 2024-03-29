;;; bits/evil.el -*- lexical-binding: t; -*-

(setq evil-split-window-below t
      evil-vsplit-window-right t
      evil-disable-insert-state-bindings t
      +evil-want-o/O-to-continue-comments nil)

(after! undo-tree
        (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode))

(map!
 :i "C-d" 'delete-char
 :i "C-y" 'xah-paste-or-paste-previous

 :i "C-j" nil
 :i "C-k" nil)

(map!
 :map (markdown-mode-map yaml-mode-map prog-mode-map)
 :n "RET" 'xah-open-file-at-cursor

 :map general-override-mode-map
 :m "H" 'evil-first-non-blank
 :m "L" 'evil-last-non-blank

 :ni "M-h" 'evil-window-left
 :ni "M-j" 'evil-window-down
 :ni "M-k" 'evil-window-up
 :ni "M-l" 'evil-window-right)

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
