;;; bits/ui.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq display-line-numbers-type nil)

(after! hl-line (setq hl-line-sticky-flag nil))

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
  :config
  (custom-theme-set-faces! 'eink
    '(region :background "peach puff")
    '(selectrum-current-candidate :background "yellow")
    '(vertico-current :background "khaki1")
    '(hl-line :inherit highlight :background "lavender")
    '(lsp-ui-sideline-code-action :foreground "limegreen")
    '(rustic-string-interpolation-face t)
    '(terraform--resource-name-face t)
    '(terraform--resource-type-face t)
    '(diredfl-file-name :inherit default)
    '(diredfl-dir-name :weight bold)
    '(diredfl-file-suffix :inherit default)
    '(diredfl-symlink :inherit default)
    '(diredfl-date-time :inherit default)
    '(diredfl-number :inherit default)
    '(diredfl-dir-heading :inherit default)
    '(diredfl-no-priv :inherit default)
    '(diredfl-dir-priv :inherit default)
    '(diredfl-read-priv :inherit default)
    '(diredfl-write-priv :inherit default)
    '(diredfl-exec-priv :inherit default)
    '(diredfl-rare-priv :inherit default)
    '(diredfl-ignored-file-name :inherit default)
    '(diredfl-compressed-file-suffix :inherit default)))

(defun fix-cursor-color ()
  (interactive)
  (if (string-equal (face-attribute 'default :foreground) "#111111")
      (setq evil-default-cursor "#111111")
    (setq evil-default-cursor (get 'cursor 'evil-normal-color))))

(defvar after-enable-theme-hook nil
  "Hook run after a color theme is enabled using `enable-theme'.")
(defadvice enable-theme (after run-after-enable-theme-hook activate)
  "Run `after-enable-theme-hook'."
  (run-hooks 'after-enable-theme-hook))

(add-hook! 'after-enable-theme-hook #'fix-cursor-color)
