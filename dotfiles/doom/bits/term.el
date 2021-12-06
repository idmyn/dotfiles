;;; bits/term.el -*- lexical-binding: t; -*-

(map!
 :map vterm-mode-map
 :i "M-<backspace>" 'vterm-send-meta-backspace
 :i "C-j" 'vterm-send-C-j
 :i "C-k" 'vterm-send-C-k)
