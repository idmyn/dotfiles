;;; bits/ui.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq display-line-numbers-type nil)
(map!
 :leader
 :prefix "t"
 :desc "Line numbers" "l" (lambda ()
                            (interactive)
                            (setq display-line-numbers-type t)
                            (if global-display-line-numbers-mode
                                (global-display-line-numbers-mode 0)
                              (global-display-line-numbers-mode 1))
                            ))

(global-hide-mode-line-mode)

;; https://magit.vc/manual/magit/The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate.html
(setq-default mode-line-format
              (delete '(vc-mode vc-mode) mode-line-format))

(setq column-number-mode t)

(after! hl-line (setq hl-line-sticky-flag nil))

(setq doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 12))

(use-package! eink-theme
  :custom
  (doom-theme 'eink)
  (doom-font "Iosevka SS09-18")
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
    '(lsp-ui-doc-background :background "lavender blush")
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
  (if (string-equal (face-attribute 'default :foreground) "#111111")
      (setq evil-default-cursor "#111111")
    (setq evil-default-cursor (get 'cursor 'evil-normal-color))))

(defvar after-enable-theme-hook nil
  "Hook run after a color theme is enabled using `enable-theme'.")
(defadvice enable-theme (after run-after-enable-theme-hook activate)
  "Run `after-enable-theme-hook'."
  (run-hooks 'after-enable-theme-hook))

(add-hook! 'after-enable-theme-hook #'fix-cursor-color)
