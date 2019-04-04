;; LOAD PACKAGES
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (ranger emmet-mode goto-chg))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; EVIL
(setq evil-want-C-u-scroll t)
(add-to-list 'load-path "~/.emacs.d/evil")

;; initialise evil-leader before evil to enable in initial buffers
(require 'evil-leader)
(global-evil-leader-mode)

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

;; leader bindings
(evil-leader/set-leader "<SPC>")

(evil-leader/set-key
  "<SPC>" 'execute-extended-command
  "f" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer
  "r" 'recentf-open-files
  "d" 'deer)


;; RANGER
(require 'ranger)
(ranger-override-dired-mode t)
(setq ranger-hide-cursor nil)


;; EMMET
(add-hook 'sgml-mode-hook 'emmet-mode) ; auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ; enable Emmet's css abbreviation.
(define-key evil-insert-state-map (kbd "C-z") 'emmet-expand-line)


;; EDITOR APPEARANCE
(require 'eink-theme)

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

;; less file info in dired
(defun xah-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'xah-dired-mode-setup)


;; ADD FUNCTIONALITY
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves/"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files) ; also <leader>r


;; EDITDOR NAVIGATION BINDINGS
(global-set-key (kbd "C-h")  'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k")    'windmove-up)
(global-set-key (kbd "C-j")  'windmove-down)

;; C-; to comment/uncomment
(define-key evil-motion-state-map (kbd "C-;") 'comment-dwim)
