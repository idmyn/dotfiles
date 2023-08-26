;;; bits/lsp.el -*- lexical-binding: t; -*-

;(use-package lsp-bridge
;  :load-path "../lisp/lsp-bridge"
;  :init (global-lsp-bridge-mode)
;  :config
;  (setq
;   lsp-bridge-enable-hover-diagnostic t)
;
;  (map!
;   :leader
;   :prefix "c"
;   :desc "Code action" "a" 'lsp-bridge-code-action)
;
;  (map! :map lsp-bridge-mode-map
;        :n "g h" 'lsp-bridge-popup-documentation
;
;        :map acm-mode-map
;        "C-j" 'acm-select-next
;        "C-k" 'acm-select-prev))
;
;(add-hook 'lsp-bridge-mode-hook #'(lambda() (setq-local +lookup-definition-functions '(lsp-bridge-find-def t)
;                                             +lookup-implementations-functions '(lsp-bridge-find-impl t)
;                                             +lookup-references-functions '(lsp-bridge-find-references t))))
;
;(setq lsp-clients-deno-enable-code-lens-implementations nil)
;
;(after! lsp-ui
;  (setq lsp-ui-doc-max-height 20)
;  (setq lsp-ui-doc-delay 0)
;
;  (map!
;   :map lsp-ui-mode-map
;   :n "g h" 'lsp-ui-doc-show))
