;;; bits/git.el -*- lexical-binding: t; -*-

(map!
 :after magit
 :map magit-mode-map
 :n "j" 'magit-section-forward
 :n "k" 'magit-section-backward)
