(setq user-full-name "David Mynors"
      user-mail-address "hello@davidmyno.rs")

;;; INITIALISE USE-PACKAGE
;; http://cachestocaches.com/2015/8/getting-started-use-package/

;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Manually installed plugins go in a 'lisp' folder
(add-to-list 'load-path "~/.emacs.d/lisp")

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))


;;; EDITOR APPEARANCE / QUALITY OF LIFE TWEAKS

(use-package eink-theme
  :ensure t
  :config ; fonts
  (defun input-sans ()
    "Set font to Input Sans Narrow in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "Input Sans Narrow"))
    (buffer-face-mode))
  (defun input-mono ()
    "Set font to Input Mono Narrow in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "Input Mono Narrow"))
    (buffer-face-mode))
  (add-hook 'prog-mode-hook 'input-sans)
  (add-hook 'ranger-mode-hook 'input-mono))

(setq inhibit-startup-screen t)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Minimal modeline
;; https://gitlab.com/mark.feller/emacs.d/blob/master/modules/module-solarized.el
(set-face-attribute 'mode-line nil
                    :background "#e6e6e1"
                    :foreground "#323232"
                    :box '(:line-width 4 :color "#e6e6e1")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#f7f7ef"
                    :foreground "#8a8a8a"
                    :box '(:line-width 4 :color "#f7f7ef")
                    :overline nil
                    :underline nil)

(define-minor-mode minor-mode-blackout-mode
  "Hides minor modes from the mode line."
  t)

(catch 'done
  (mapc (lambda (x)
          (when (and (consp x)
                     (equal (cadr x) '("" minor-mode-alist)))
            (let ((original (copy-sequence x)))
              (setcar x 'minor-mode-blackout-mode)
              (setcdr x (list "" original)))
            (throw 'done t)))
        mode-line-modes))

;; File path in title bar
;; https://stackoverflow.com/a/29821453
(setq frame-title-format
      '(buffer-file-name "%b - %f" ; File buffer
        (dired-directory dired-directory ; Dired buffer
         (revert-buffer-function "%b" ; Buffer Menu
          ("%b - Dir: " default-directory))))) ; Plain buffer

(global-display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)
(use-package avy
  :ensure t
  :config (setq avy-timeout-seconds 0.3))

(show-paren-mode 1)
(electric-pair-mode 1)

;; Eighty Column Rule
(require 'whitespace)
(setq whitespace-line-column 80
      whitespace-style '(face tabs tab-mark lines-tail trailing))

(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#fffff8" :foreground "#111111" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width condensed :foundry "nil" :family "Input Sans Narrow"))))
 '(line-number ((t (:inherit (shadow default) :family "Input Mono Narrow"))))
 '(whitespace-tab ((t (:foreground "#9e9e9e")))))
(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ;; use pipe char to indicate tab

(global-whitespace-mode t)
;; while we're at it...
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Indentation
(use-package aggressive-indent
  :ensure t)
(setq-default indent-tabs-mode nil)
(setq backward-delete-char-untabify-method nil)
(setq-default electric-indent-inhibit nil)

(custom-set-variables
 '(projectile-globally-ignored-files (quote ("TAGS" ".DS_Store" ".learn" ".rspec" ".gitignore")))
 '(show-paren-mode t)
 '(smie-config (quote ((css-mode (2 :elem basic 4)))))
 '(tool-bar-mode nil))

;; https://github.com/antonj/Highlight-Indentation-for-Emacs
(use-package highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "#f7f7ef")
  (add-hook 'ruby-mode-hook 'highlight-indentation-mode))

;; Filesystem hygiene
;; https://www.emacswiki.org/emacs/BackupFiles
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

;; Function for commenting line/region
;; https://stackoverflow.com/a/9697222
(defun comment-or-uncomment-region-or-line ()
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;; Separate evil clipboard from system clipboard
;; https://github.com/rolandwalker/simpleclip
(require 'simpleclip)
(simpleclip-mode 1)

;; Easier daemon restart for config changes
(use-package restart-emacs
  :ensure t)

;; Flatiron School niceties
(defun learn-tests ()
  "Run learn tests in `shell' buffer."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
  (comint-send-string
   (get-buffer-process (shell))
   "learn\n")))


;;; EDITOR NAVIGATION / INTERACTION

;; General (keybindings)
(use-package general
  :ensure t
  :after evil
  :config
  ;; (general-swap-key nil 'motion
  ;;   ";" ":")

  ;; global bindings
  (general-define-key
    "M-j" 'windmove-left
    "M-k" 'windmove-down
    "M-l" 'windmove-up
    "M-;" 'windmove-right

    "C-SPC" 'complete-symbol
    "C-j" 'avy-goto-char-timer
    "C-;" 'avy-goto-line)

  ;; https://www.emacswiki.org/emacs/RecreateScratchBuffer
  (defun switch-to-scratch-and-back ()
      "Toggle between *scratch* buffer and the current buffer.
      If the *scratch* buffer does not exist, create it."
      (interactive)
      (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
          (if (equal (current-buffer) scratch-buffer-name)
              (switch-to-buffer (other-buffer))
              (switch-to-buffer scratch-buffer-name (lisp-interaction-mode)))))

  (general-create-definer global-leader
    :prefix "SPC")
  (global-leader 'motion 'override
    ;; "f" 'swiper
    ;; "x" 'counsel-M-x
    "b" 'switch-to-buffer
    "n" 'deer
    "RET" 'window-swap-states
    ;; "s" 'switch-to-scratch-and-back ; causing trouble with flycheck
    "s" 'swiper
    "w" 'save-buffer
    "e" 'eshell
    "g" 'magit-status
    "i" 'aggressive-indent-indent-defun
    "h" 'highlight-indentation-mode
    "c" 'comment-or-uncomment-region-or-line
    "q" 'evil-quit
    "v" 'split-window-right
    "x" 'split-window-below
    "p" 'projectile-command-map
    "l" 'learn-tests
    "a" 'cheat-sh
    "u" 'undo-tree-visualize
    "r" 'query-replace)

  (general-create-definer local-leader
    :prefix "m")
    ;; "l" for lookup, "b" for breakpoint, "d" for debug, "e" for evaluate

  (general-def 'normal
    "s" 'avy-goto-char-timer
    "J" nil ; unbind from evil-join
    "p" nil ; unbind from evil-paste-after
    ">" 'evil-shift-right-line
    "<" 'evil-shift-left-line)

  (general-def 'visual
    ">" 'evil-shift-right
    "<" 'evil-shift-left)

  (require 'move-border)
  (general-def 'motion
    "j" 'evil-backward-char
    "k" 'evil-next-line
    "l" 'evil-previous-line
    ";" 'evil-forward-char

    "h" 'evil-paste-after
    "H" 'evil-paste-before
    ;; "p" 'evil-ex
    "p" 'projectile-command-map

    "RET" 'other-window

    "C-e" 'er/expand-region

    ;; ")" 'evil-beginning-of-line

    ;; easier motion around lines and paragraphs
    "J" 'evil-first-non-blank
    "K" 'forward-paragraph
    "L" 'backward-paragraph
    ":" 'evil-last-non-blank

    "M-u" 'move-border-left
    "M-i" 'move-border-down
    "M-o" 'move-border-up
    "M-p" 'move-border-right)

  ;; emacs bindings in insert mode
  ;; https://github.com/warchiefx/dotemacs/blob/master/site-wcx/wcx-evil.el
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map
    (read-kbd-macro evil-toggle-key) 'evil-emacs-state)
  ;; fix escape key
  (use-package evil-escape
    :ensure t
    :config
    (evil-escape-mode)
    (global-set-key (kbd "<escape>") 'evil-escape)))

;; Evil
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (use-package undo-tree
    :ensure t
    :config (global-undo-tree-mode))
  (use-package expand-region
    :ensure t)

  :config
  (evil-mode 1)
  (setq-default evil-shift-width 2)

  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode 1)))

;; Ivy
(use-package ivy
  :ensure t
  :init ;; use flx if ivy--regex-fuzzy
  (use-package flx
    :ensure t)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy)))
  (global-set-key "\C-s" 'swiper)

  (general-def
    :keymaps '(ivy-minibuffer-map swiper-map)
    "C-j" (kbd "DEL")
    "C-k" 'ivy-next-line
    "C-l" 'ivy-previous-line
    "C-;" 'ivy-alt-done)

  (use-package counsel
    :ensure t
    :config (counsel-mode 1)))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/Development/"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'native) ; seems to run quicker than 'alien'

  (projectile-register-project-type 'learn '(".learn")
                                    :test-suffix "_spec")

  (projectile-register-project-type 'python '("RPGtodo.py"))

  (projectile-mode +1))

;; Company
(use-package company
  :config
  (company-tng-configure-default) ; tab 'n' go
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.3)
  (global-company-mode 1))
(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

;; Hyperbole
(use-package hyperbole
  :ensure t)

;; Ranger
(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-hide-cursor nil)
  (add-hook 'ranger-mode-hook 'ranger-toggle-dotfiles)
  (general-def 'motion ranger-mode-map
    "." 'ranger-toggle-dotfiles
    "r" 'wdired-change-to-wdired-mode

    "j" 'ranger-up-directory
    "k" 'ranger-next-file
    "l" 'ranger-prev-file
    ";" 'ranger-find-file))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (setq flycheck-global-modes '(not emacs-lisp-mode)
        flycheck-check-syntax-automatically '(mode-enabled save)))

;; LSP
(use-package lsp-mode
  :ensure t
  :hook ((ruby-mode js-mode) . lsp)
  :commands lsp
  :config
  (setq
   lsp-auto-guess-root t
   lsp-prefer-flymake nil
   lsp-ui-flycheck-live-reporting nil
   lsp-enable-snippet nil
   lsp-ui-doc-enable nil
   lsp-ui-peek-enable nil
   lsp-ui-sideline-enable nil
   lsp-ui-imenu-enable nil))

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t
  :config
  (dap-mode 1)
  (dap-ui-mode 1))
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package cheat-sh) ; https://github.com/davep/cheat-sh.el


;;; LANGUAGE/MODE SPECIFIC

;; Magit
(use-package magit
  :ensure t
  :config
  (with-eval-after-load 'evil
    (add-to-list 'evil-insert-state-modes 'magit-status-mode)
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)))

;; Eshell
(defun eshell-setup-keys() ; implementation inspired by evil-collection
  "Set up `evil' bindings for `eshell'."
  (general-def 'insert eshell-mode-map
    "C-k" 'eshell-next-matching-input-from-input
    "C-l" 'eshell-previous-matching-input-from-input))
(add-hook 'eshell-first-time-mode-hook 'eshell-setup-keys)

(use-package load-bash-alias
  :ensure t
  :config
  (setq load-bash-alias-bashrc-file "~/.aliases"))
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)
(setq eshell-history-size 1000000)
(setq explicit-shell-file-name "/bin/bash") ; for cases where I can't use eshell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; TXT/ORG
(add-hook 'org-mode-hook (lambda () (electric-quote-mode 1)))

;; HTML/CSS
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ; auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ; enable Emmet's css abbreviation.
  (general-def 'insert
    "C-z" 'emmet-expand-line))

;; Javascript
(use-package dap-node
  :config
  (setq js-indent-level 2)
  (local-leader 'normal js-mode-map
    "d" 'dap-debug
    "b" 'dap-breakpoint-toggle
    "e" 'dap-eval-thing-at-point)
  (local-leader 'visual js-mode-map
    "e" 'dap-eval-region))

;; Ruby
(use-package dap-ruby
  :config
  (setq ruby-indent-level 2)
  (local-leader 'normal ruby-mode-map
    "l" 'yari
    "d" 'dap-debug
    "b" 'dap-breakpoint-toggle
    "e" 'dap-eval-thing-at-point
    "r" 'inf-ruby)
  (local-leader 'visual ruby-mode-map
    "e" 'dap-eval-region))

(use-package chruby
  :ensure t
  :config (chruby "2.6.3"))

(use-package inf-ruby
  :ensure t
  :interpreter "ruby")

(use-package yari
  :ensure t
  :interpreter "ruby")

(use-package rubocop
  :ensure t
  :interpreter "ruby")

;; Python
;; https://jonathanabennett.github.io/blog/2019/06/20/python-and-emacs-pt.-1/
(use-package elpy
  :ensure t
  :custom
  (elpy-rpc-backend "jedi"))
(use-package python
  :config
  (setq python-indent-offset 4)
  (elpy-enable))
(use-package company-jedi
  :ensure t
  :defer t
  :init
  (defun enable-jedi()
    (setq-local company-backends
                (append '(company-jedi) company-backends)))
  (with-eval-after-load 'company
    (add-hook 'python-mode-hook 'enable-jedi)))


;;; macOS SPECIFIC

;; set the path variable (important for macOS?)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Allow hash to be entered on UK macbook keyboard layout
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
