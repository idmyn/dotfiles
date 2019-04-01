;; LOAD PACKAGES
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (emmet-mode goto-chg))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; EVIL
(setq evil-want-C-u-scroll t)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(require 'undo-tree)
(global-undo-tree-mode)

;; swap colon and semicolon
(define-key evil-motion-state-map ";" 'evil-ex)
(define-key evil-motion-state-map ":" 'evil-repeat-find-char)

;; easier motion within lines
(define-key evil-motion-state-map "H" 'evil-first-non-blank)
(define-key evil-motion-state-map "L" 'evil-last-non-blank)
(define-key evil-motion-state-map "K" 'evil-window-top)
(define-key evil-motion-state-map "J" 'evil-window-bottom)


;; EMMET
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(define-key evil-insert-state-map (kbd "C-z") 'emmet-expand-line)


;; EDITOR LAYOUT
(setq inhibit-startup-screen t)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq-default mode-line-format nil)

(global-display-line-numbers-mode)
(setq-default display-line-numbers-type 'relative)

(require 'eink-theme)


;; ADD FUNCTIONALITY
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(add-to-list 'write-file-functions 'delete-trailing-whitespace)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)


;; EDITDOR NAVIGATION BINDINGS
(global-set-key (kbd "C-h")  'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k")    'windmove-up)
(global-set-key (kbd "C-j")  'windmove-down)

;; spacebar for M-x
(define-key evil-motion-state-map " " 'execute-extended-command)
