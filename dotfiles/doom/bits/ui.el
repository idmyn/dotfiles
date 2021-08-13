;;; bits/ui.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq display-line-numbers-type nil)

(use-package! eink-theme
  :custom
  (doom-theme 'eink)
  (doom-font "Iosevka SS09-15")
  ;; File path in title bar
  ;; https://stackoverflow.com/a/29821453
  (frame-title-format
   '(buffer-file-name "%b - %f" ; File buffer
        	      (dired-directory dired-directory ; Dired buffer
        			       (revert-buffer-function "%b" ; Buffer Menu
        						       ("%b - Dir: " default-directory))))) ; Plain buffer
  :custom-face
  (region ((t (:background "peach puff"))))
  (selectrum-current-candidate ((t (:background "yellow"))))
  (vertico-current ((t (:background "khaki1"))))
  (hl-line ((t (:inherit highlight :background "lavender"))))
  (lsp-ui-sideline-code-action ((t (:foreground "limegreen"))))
  (rustic-string-interpolation-face ((t)))
  (terraform--resource-name-face ((t)))
  (terraform--resource-type-face ((t)))
  (diredfl-file-name ((t)))
  (diredfl-file-suffix ((t)))
  (diredfl-symlink ((t)))
  (diredfl-date-time ((t)))
  (diredfl-number ((t)))
  (diredfl-dir-heading ((t)))
  (diredfl-no-priv ((t)))
  (diredfl-dir-priv ((t)))
  (diredfl-read-priv ((t)))
  (diredfl-write-priv ((t)))
  (diredfl-exec-priv ((t)))
  (diredfl-rare-priv ((t)))
  (diredfl-ignored-file-name ((t)))
  (diredfl-compressed-file-suffix ((t)))
  (diredfl-dir-name ((t (:weight bold)))))
