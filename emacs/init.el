;;; init.el - http://endlessparentheses.com/init-org-Without-org-mode.html
(defvar endless/init.org-message-depth 3
  "What depth of init.org headers to message at startup.")

(with-temp-buffer
  (insert-file "~/.emacs.d/init.org")
  (goto-char (point-min))
  (while (not (eobp))
    (forward-line 1)
    (cond
     ;; Report Headers
     ((looking-at
       (format "\\* +.*$"
               endless/init.org-message-depth))
      (message "%s" (match-string 0)))
     ;; Evaluate Code Blocks
     ((looking-at "^#\\+BEGIN_SRC +emacs-lisp *$")
      (let ((l (match-end 0)))
        (search-forward "\n#+END_SRC")
        (eval-region l (match-beginning 0))))
     ;; Finish on the next level-1 header
      ((looking-at "^\\* ")
       (goto-char (point-max))))))

(custom-set-variables
 '(emojify-display-style (quote image))
 '(emojify-download-emojis-p (quote ask))
 '(emojify-emoji-set "twemoji-v2-22")
 '(emojify-emoji-styles (quote (unicode)))
 '(global-emojify-mode t)
 '(js2-mode-show-parse-errors nil)
 '(projectile-globally-ignored-files (quote ("TAGS" ".DS_Store" ".learn" ".rspec" ".gitignore")))
 '(show-paren-mode t)
 '(smie-config (quote ((css-mode (2 :elem basic 4)))))
 '(tool-bar-mode nil))

(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#fffff8" :foreground "#111111" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width condensed :foundry "nil" :family "Input Sans Narrow"))))
 '(deft-summary-face ((t nil)))
 '(deft-title-face ((t (:inherit font-lock-function-name-face :weight bold))))
 '(js2-error ((t nil)))
 '(js2-warning ((t nil)))
 '(line-number ((t (:inherit (shadow default) :family "Input Mono Narrow"))))
 '(org-block-begin-line ((t (:height 0.8))))
 '(vterm-color-black ((t (:inherit term-color-black :background "dark gray"))))
 '(web-mode-doctype-face ((t nil)))
 '(web-mode-html-attr-name-face ((t nil)))
 '(web-mode-html-tag-face ((t nil)))
 '(web-mode-json-key-face ((t nil)))
 '(web-mode-symbol-face ((t nil)))
 '(whitespace-tab ((t (:foreground "#9e9e9e")))))
