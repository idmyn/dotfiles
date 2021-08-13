;;; bits/completion.el -*- lexical-binding: t; -*-

(after! orderless
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism orderless-flex)))

(setq consult-buffer-filter '("^ " "\\*.*\\*"))
