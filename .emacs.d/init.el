;; http://cachestocaches.com/2015/8/getting-started-use-package/

;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))


;; IVY
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)

  (use-package counsel
    :ensure t
    :config (counsel-mode 1)))


;; EVIL
(setq evil-want-C-u-scroll t)
(use-package evil
  :ensure t
  :config

  ;; initialise evil-leader before evil to enable in initial buffers
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      "x" 'counsel-M-x
      "f" 'counsel-find-file
      "b" 'switch-to-buffer
      "d" 'deer))

  (evil-mode 1)

  (use-package undo-tree
    :ensure t
    :config (global-undo-tree-mode))

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
    (global-set-key (kbd "<escape>") 'evil-escape))

  ;; swap colon and semicolon
  (define-key evil-motion-state-map ";" 'evil-ex)
  (define-key evil-motion-state-map ":" 'evil-repeat-find-char)

  ;; easier motion within lines
  (define-key evil-motion-state-map "H" 'evil-first-non-blank)
  (define-key evil-motion-state-map "L" 'evil-last-non-blank)
  (define-key evil-motion-state-map "K" 'evil-window-top)
  (define-key evil-motion-state-map "J" 'evil-window-bottom))


;; RANGER
(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-hide-cursor nil))


;; EMMET
(add-hook 'sgml-mode-hook 'emmet-mode) ; auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ; enable Emmet's css abbreviation.
(define-key evil-insert-state-map (kbd "C-z") 'emmet-expand-line)


;; EDITOR APPEARANCE
(use-package eink-theme
  :ensure t)

(setq inhibit-startup-screen t)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq-default mode-line-format nil)

(global-display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)

;; Eighty Column Rule
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face tabs lines-tail trailing))
(global-whitespace-mode t)
;; while we're at it...
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;; ADD FUNCTIONALITY
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; C-; to comment/uncomment
(define-key evil-motion-state-map (kbd "C-;") 'comment-dwim)
