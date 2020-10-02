(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(show-paren-mode 1)
(electric-pair-mode 1)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Iosevka SS09-15"))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq use-package-enable-imenu-support t)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(use-package exec-path-from-shell
  :config
  (when (daemonp)
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize)))

(use-package eink-theme
  :config
  (column-number-mode)
  :custom-face
  (region ((t (:background "peach puff"))))
  (hl-line ((t (:inherit highlight :background "lavender"))))
  (lsp-ui-sideline-code-action ((t (:foreground "limegreen"))))
  (selectrum-current-candidate ((t (:background "peach puff"))))
  (rustic-string-interpolation-face ((t)))
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

(load-theme 'eink)

(use-package general
  :config
  (general-create-definer global-leader
    :prefix "SPC")
  (general-create-definer toggle-leader
    :prefix "SPC t")
  (general-create-definer file-leader
    :prefix "SPC f")

  (file-leader 'normal
    "s" 'save-buffer)

  (general-define-key
   "M-h" 'windmove-left
   "M-j" 'windmove-down
   "M-k" 'windmove-up
   "M-l" 'windmove-right))

(use-package magit
  :config
  (use-package diff-hl
    :config (global-diff-hl-mode +1))
  (global-auto-revert-mode t) ; buffers should change when branch changes
  (global-leader 'normal
    "g" 'magit))

(use-package evil
  :init (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (setq evil-insert-state-map (make-sparse-keymap))
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)

  (global-leader 'normal
    "q" 'evil-quit
    "Q" 'save-buffers-kill-emacs
    "v" 'evil-window-vsplit
    "s" 'evil-window-split
    "`" 'evil-switch-to-windows-last-buffer)

  (general-def 'motion
    "H" 'evil-first-non-blank
    "L" 'evil-last-non-blank
    "g c" 'comment-dwim)

  (use-package better-jumper
    :config
    (better-jumper-mode +1)
    (with-eval-after-load 'evil-maps
      (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-forward)
      (define-key evil-motion-state-map (kbd "TAB") 'better-jumper-jump-backward)))

  (use-package evil-surround
    :config (global-evil-surround-mode 1))

  (use-package evil-magit
    :config
    (add-to-list 'evil-insert-state-modes 'magit-status-mode)
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)

    (general-def 'normal 'magit-status-mode-map
      "j" 'magit-section-forward
      "k" 'magit-section-backward)))

(use-package dumb-jump
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package ranger
  :config
  (ranger-override-dired-mode t)
  (setq ranger-hide-cursor nil)
  (add-hook 'ranger-mode-hook 'ranger-toggle-dotfiles)
  (file-leader 'normal
    "d" 'ranger)
  (general-def 'motion ranger-mode-map
    "." 'ranger-toggle-dotfiles
    "w" 'wdired-change-to-wdired-mode))

(use-package selectrum
  :config
  (selectrum-mode +1)

  (general-def selectrum-minibuffer-map
    "C-j" 'selectrum-next-candidate
    "C-k" 'selectrum-previous-candidate)

  (recentf-mode +1)
  (defun recentf-open-files+ ()
    "Use `completing-read' to open a recent file."
    (interactive)
    (let ((files (mapcar 'abbreviate-file-name recentf-list)))
      (find-file (completing-read "Find recent file: " files nil t))))

  (global-leader 'normal
    "b" 'switch-to-buffer)
  (file-leader 'normal
    "r" 'recentf-open-files+)

  (use-package prescient
    :config
    (use-package selectrum-prescient
      :config
      (setq prescient-filter-method 'fuzzy)
      (selectrum-prescient-mode +1)
      (prescient-persist-mode +1))))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode))

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
  (global-leader 'normal
    "p" 'projectile-command-map
    "SPC" 'projectile-find-file))

(use-package lsp-mode
  :hook ((go-mode . lsp))
  :commands lsp)

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-global-modes '(not emacs-lisp-mode))
  (use-package flycheck-title
    :config (flycheck-title-mode)))

(use-package go-mode
  :mode "\\.go\\'")

(use-package markdown-mode
  :mode "\\.md\\'"
  :config (setq-default fill-column 80)
  :custom-face
  (markdown-markup-face ((t (:inherit default :foreground "gray50"))))
  (markdown-pre-face ((t (:inherit default)))))

(use-package olivetti
  :commands olivetti-mode
  :init
  (toggle-leader 'normal
    "o" 'olivetti-mode))

(setq js-indent-level 2)

(use-package terraform-mode
  :mode "\\.tf\\'"
  :config (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))

(use-package yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode) ("\\.yml\\'" . yaml-mode)))
