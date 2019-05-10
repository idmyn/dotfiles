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
  :ensure t)

(setq inhibit-startup-screen t)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq-default mode-line-format nil)

(global-display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)

(show-paren-mode 1)
(electric-pair-mode 1)

;; Eighty Column Rule
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs lines-tail trailing))
(global-whitespace-mode t)
;; while we're at it...
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


;;; EDITOR NAVIGATION / INTERACTION

;; General (keybindings)
(use-package general
  :ensure t
  :after evil
  :config
  (general-swap-key nil 'motion
    ";" ":")

  (general-create-definer my-leader-def
    :prefix "SPC")
  (my-leader-def 'motion 'override
      "x" 'counsel-M-x
      "f" 'counsel-find-file
      "b" 'switch-to-buffer
      "d" 'deer
      "s" 'shell
      "e" 'eshell)

  (require 'move-border)
  (define-key evil-normal-state-map "J" nil) ; unbind from evil-join
  (general-def 'motion
    ;; easier motion around lines and paragraphs
    "H" 'evil-first-non-blank
    "L" 'evil-last-non-blank
    "K" 'backward-paragraph
    "J" 'forward-paragraph

    "M-h" 'windmove-left
    "M-j" 'windmove-down
    "M-k" 'windmove-up
    "M-l" 'windmove-right

    "M-y" 'move-border-left
    "M-u" 'move-border-down
    "M-i" 'move-border-up
    "M-o" 'move-border-right

    "C-;" 'comment-dwim)

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
  :config
  (evil-mode 1)
  (use-package undo-tree
    :ensure t
    :config (global-undo-tree-mode)))

;; Ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (global-set-key "\C-s" 'swiper)

  (use-package counsel
    :ensure t
    :config (counsel-mode 1)))

;; Ranger
(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-hide-cursor nil)
  (general-def 'motion ranger-mode-map
    "." 'ranger-toggle-dotfiles))


;;; LANGUAGE/MODE SPECIFIC

;; Eshell aliases and autosuggest
(use-package load-bash-alias
  :ensure t
  :config
  (setq load-bash-alias-bashrc-file "~/.aliases"))
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)

;; TXT/ORG
(add-hook 'org-mode-hook (lambda () (electric-quote-mode 1)))

;; HTML/CSS
(add-hook 'sgml-mode-hook 'emmet-mode) ; auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ; enable Emmet's css abbreviation.
(define-key evil-insert-state-map (kbd "C-z") 'emmet-expand-line)

;; Ruby
(use-package chruby
  :ensure t
  :config (chruby "2.6.3"))

(use-package inf-ruby
  :ensure t
  :interpreter "ruby")

(use-package seeing-is-believing
  :ensure t
  :interpreter "ruby"
  :config
  (setq seeing-is-believing-executable "/Users/david/.rbenv/shims/seeing_is_believing"))


;;; macOS SPECIFIC
;; set the path variable (important for macOS?)
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Allow hash to be entered on UK macbook keyboard layout
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
