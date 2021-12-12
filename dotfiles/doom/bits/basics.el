;;; bits/basics.el -*- lexical-binding: t; -*-

(map! :when IS-MAC
      :i "M-3" (lambda () (interactive) (insert "#")))

(map!
 "s--" 'text-scale-decrease
 "s-=" 'text-scale-increase)

(after! windmove
  (require 'framemove)
  (setq framemove-hook-into-windmove t))

(use-package! ace-window
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (:map general-override-mode-map
         ("M-o" . ace-window)))
