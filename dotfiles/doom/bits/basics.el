;;; bits/basics.el -*- lexical-binding: t; -*-

(map! :when IS-MAC
      :i "M-3" (lambda () (interactive) (insert "#")))

(map!
 "s--" 'text-scale-decrease
 "s-=" 'text-scale-increase)
