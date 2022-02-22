;;; bits/lsp.el -*- lexical-binding: t; -*-

(setq lsp-clients-deno-enable-code-lens-implementations nil)

(after! lsp-ui
  (setq lsp-ui-doc-max-height 20)
  (setq lsp-ui-doc-delay 0)

  (map!
   :map lsp-ui-mode-map
   :n "g h" 'lsp-ui-doc-show))
