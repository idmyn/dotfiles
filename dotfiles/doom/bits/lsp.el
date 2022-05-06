;;; bits/lsp.el -*- lexical-binding: t; -*-

(setq lsp-clients-deno-enable-code-lens-implementations nil)

(after! lsp-ui
  (setq lsp-ui-doc-max-height 20)
  (setq lsp-ui-doc-delay 0)

  (map!
   :map lsp-ui-mode-map
   :n "g h" 'lsp-ui-doc-show))

;; (defvar-local my/flycheck-local-cache nil)
;;
;; (defun my/flycheck-checker-get (fn checker property)
;;   (or (alist-get property (alist-get checker my/flycheck-local-cache))
;;       (funcall fn checker property)))
;;
;; (advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)
;;
;; (add-hook 'lsp-managed-mode-hook
;;           (lambda ()
;;             (when (derived-mode-p 'typescript-mode)
;;               (setq my/flycheck-local-cache '((lsp . ((next-checkers . (javascript-eslint))))))))
