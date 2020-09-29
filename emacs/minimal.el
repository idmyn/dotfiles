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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

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
  (use-package diff-hl)
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
    "v" 'evil-window-vsplit
    "s" 'evil-window-split)

  (general-def 'motion
    "H" 'evil-first-non-blank
    "L" 'evil-last-non-blank
    "g c" 'comment-dwim)

  (use-package evil-magit
    :config
    (add-to-list 'evil-insert-state-modes 'magit-status-mode)
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)

    (general-def 'normal 'magit-status-mode-map
    "j" 'magit-section-forward
    "k" 'magit-section-backward)))

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
    "r" 'recentf-open-files+
    "b" 'switch-to-buffer)

  (use-package prescient
    :config
    (use-package selectrum-prescient
      :config
      (setq prescient-filter-method 'fuzzy)
      (selectrum-prescient-mode +1)
      (prescient-persist-mode +1))))

(use-package projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
  (global-leader 'normal
    "p" 'projectile-command-map
    "SPC" 'projectile-find-file))

(use-package eglot
  :config
  (use-package flymake-diagnostic-at-point
    :after flymake
    :config
    (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))
  (add-hook 'go-mode-hook 'eglot-ensure))

(use-package go-mode
  :mode "\\.go\\'")
