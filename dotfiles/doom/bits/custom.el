;;; custom.el --- Description -*- lexical-binding: t; -*-

(custom-set-variables
 '(safe-local-variable-values
   '((lsp-enabled-clients deno-ls)
     (+format-with . prettier)
     (eval . (format-all-mode t))
     (eval . (prettier-mode t)))))
