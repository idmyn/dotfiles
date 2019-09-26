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
  (add-hook 'sql-mode-hook 'input-mono)
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
;; (setq-default display-line-numbers-type 'relative)
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
 '(org-block-begin-line ((t (:height 0.8))))
 '(web-mode-doctype-face ((t nil)))
 '(web-mode-html-attr-name-face ((t nil)))
 '(web-mode-html-tag-face ((t nil)))
 '(web-mode-json-key-face ((t nil)))
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
  (add-hook 'web-mode-hook 'highlight-indentation-mode)
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

;; Flatiron School niceties
(defun learn-tests ()
  "Run learn tests in `shell' buffer."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
  (comint-send-string
   (get-buffer-process (shell))
   "learn --f-f\n")))

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell.
Output is printed to buffer “*xah-run output*”.

The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, golang, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2018-10-12"
  (interactive)
  (let (
        ($outputb "*xah-run output*")
        (resize-mini-windows nil)
        ($suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "go run")
           ("hs" . "runhaskell")
           ("js" . "node")
           ("mjs" . "node --experimental-modules ")
           ("ts" . "tsc") ; TypeScript
           ("tsx" . "tsc")
           ("sh" . "bash")
           ("clj" . "java -cp ~/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("java" . "javac")
           ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
           ))
        $fname
        $fSuffix
        $prog-name
        $cmd-str)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq $fname (buffer-file-name))
    (setq $fSuffix (file-name-extension $fname))
    (setq $prog-name (cdr (assoc $fSuffix $suffix-map)))
    (setq $cmd-str (concat $prog-name " \""   $fname "\" &"))
    (run-hooks 'xah-run-current-file-before-hook)
    (cond
     ((string-equal $fSuffix "el")
      (load $fname))
     ((or (string-equal $fSuffix "ts") (string-equal $fSuffix "tsx"))
      (if (fboundp 'xah-ts-compile-file)
          (progn
            (xah-ts-compile-file current-prefix-arg))
        (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str $outputb ))
          (error "No recognized program file suffix for this file."))))
     ((string-equal $fSuffix "go")
      (xah-run-current-go-file))
     ((string-equal $fSuffix "java")
      (progn
        (shell-command (format "java %s" (file-name-sans-extension (file-name-nondirectory $fname))) $outputb )))
     (t (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str $outputb ))
          (error "No recognized program file suffix for this file."))))
    (run-hooks 'xah-run-current-file-after-hook)))


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

    "s-=" 'text-scale-increase
    "s--" 'text-scale-decrease

    "C-SPC" 'ivy-yasnippet)

    (general-define-key (kbd "<C-return>") 'xah-run-current-file)

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
    "f" 'switch-to-buffer
    "d" 'dumb-jump-go
    "b" 'dumb-jump-back
    "n" 'deer
    "RET" 'window-swap-states
    ;; "s" 'switch-to-scratch-and-back ; causing trouble with flycheck
    "s" 'counsel-ag
    "w" 'save-buffer
    "e" 'eshell
    "g" 'magit-status
    "i" 'aggressive-indent-indent-defun
    "h" 'highlight-indentation-mode
    "c" 'comment-or-uncomment-region-or-line
    "q" 'evil-quit
    "v" (lambda () (interactive)(split-window-right) (other-window 1))
    "x" (lambda () (interactive)(split-window-below) (other-window 1))
    "p" 'projectile-command-map
    "l" 'learn-tests
    "a" 'howdoyou-query
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

  (general-def 'insert global-map
    "C-j" 'left-char
    "C-k" 'next-line
    "C-l" 'previous-line
    "C-;" 'right-char)

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

  (use-package counsel
    :ensure t
    :config (counsel-mode 1))

  (general-def
    :keymaps '(ivy-minibuffer-map swiper-map counsel-ag-map)
    "C-j" (kbd "DEL")
    "C-k" 'ivy-next-line
    "C-l" 'ivy-previous-line
    "C-;" 'ivy-alt-done))

;; Projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-project-search-path '("~/Development/"))
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-indexing-method 'native) ; seems to run quicker than 'alien'
  (setq projectile-enable-caching t) ; ripgrep config from https://emacs.stackexchange.com/a/29200

;;; Default rg arguments
  ;; https://github.com/BurntSushi/ripgrep
  (when (executable-find "rg")
    (progn
      (defconst modi/rg-arguments
        `("--line-number"                     ; line numbers
          "--smart-case"
          "--follow"                          ; follow symlinks
          "--mmap")                           ; apply memory map optimization when possible
        "Default rg arguments used in the functions in `projectile' package.")
      (defun modi/advice-projectile-use-rg ()
        "Always use `rg' for getting a list of all files in the project."
        (mapconcat 'identity
                   (append '("\\rg") ; used unaliased version of `rg': \rg
                           modi/rg-arguments
                           '("--null" ; output null separated results,
                             "--files")) ; get file names matching the regex '' (all files)
                   " "))
      (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-rg)))

  (projectile-register-project-type 'learn '(".learn")
                                    :test-suffix "_spec")

  (projectile-register-project-type 'python '("RPGtodo.py"))

  (projectile-register-project-type 'jekyll '(".jekyll-metadata"))

  (projectile-mode +1))

;; Dumb-jump
(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg))

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

;; YASnippet
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (use-package ivy-yasnippet
    :ensure t
    :init
    (use-package dash
      :ensure t))
  (yas-global-mode 1)
  :config
  (general-def 'insert yas-minor-mode-map
    "M-RET" 'yas-expand))

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
  :hook ((js-mode) . lsp)
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

(use-package howdoyou
  :ensure t
  :config
  ;; keybinds for specific buffer names?
  ;; (general-def
  ;;                     :definer 'minor-mode
  ;;                     :states 'normal
  ;;                     :keymaps howdoyou-mode
  ;;   "C-n" 'howdoyou-next-link
  ;;   "C-p" 'howdoyou-previous-link)
  ;; (local-leader 'howdoyou-mode-map
  ;;   "p" 'howdoyou-previous-link
  ;;   "n" 'howdoyou-next-link)
  )


;;; LANGUAGE/MODE SPECIFIC

;; Magit
(use-package magit
  :ensure t
  :config
  (with-eval-after-load 'evil
    (add-to-list 'evil-insert-state-modes 'magit-status-mode)
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)))
(global-auto-revert-mode t) ; buffers should change when branch changes

;; Eshell
(defun eshell-setup-keys() ; implementation inspired by evil-collection
  "Set up `evil' bindings for `eshell'."
  (general-def 'insert eshell-mode-map
    "C-k" 'eshell-next-matching-input-from-input
    "C-l" 'eshell-previous-matching-input-from-input
    "C-;" 'eshell-send-input))
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
(general-def 'insert shell-mode-map
    "C-k" 'comint-next-input
    "C-l" 'comint-previous-input
    "C-;" 'comint-send-input)
(general-def 'normal shell-mode-map
    "C-d" 'evil-scroll-down)

;; TXT/ORG
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook (lambda () (electric-quote-mode 1)))
(use-package org
  :ensure t
  :config
  (general-def 'insert org-mode-map
    "C-j" 'org-metaleft
    "C-;" 'org-metaright)
  (general-def 'motion org-mode-map
    "C-k" 'org-metadown
    "C-l" 'org-metaup)
  ;; couldn’t get the following bindings working with general.el unfortunately
  (define-key org-mode-map (kbd "<C-return>") 'org-meta-return)
  (define-key org-mode-map (kbd "<M-return>") 'org-insert-heading-respect-content))

;; HTML/CSS
(use-package web-mode
  :ensure t
  :config
  (general-def 'web-mode-map
    "M-;" nil)
  (setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ; auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ; enable Emmet's css abbreviation.
  (general-def 'insert
    "C-z" 'emmet-expand-line))
(use-package evil-matchit
  :ensure t
  :config (global-evil-matchit-mode 1))

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
(use-package robe
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (push 'company-robe company-backends)

  (setq ruby-indent-level 2)
  (local-leader 'normal ruby-mode-map
    "d" 'robe-doc ; d for documentation
    "r" 'inf-ruby ; r for REPL
    "p" 'pry-intercept ; p for pry
    "b" 'ruby-send-buffer
    "l" 'ruby-send-line

    "t f" 'rspec-verify
    "t a" 'rspec-verify-all
    "t t" 'rspec-toggle-spec-and-target)
  (local-leader 'visual ruby-mode-map
    "v" 'ruby-send-region))

;; I only want to be promped to start the server when I open ruby files
(add-hook 'find-file-hook 'open-rb-hook)
(defun open-rb-hook ()
  (when (string= (file-name-extension buffer-file-name) "rb")
    (robe-start)))

(use-package rspec-mode
  :ensure t
  :config
  (setq rspec-use-rvm t)
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))

  (ad-activate 'rspec-compile)

  (general-def))

(use-package rvm
  :ensure t
  :config (rvm-use-default))

(use-package inf-ruby
  :ensure t
  :interpreter "ruby"
  :config
  ;; https://github.com/dgutov/robe#integration-with-rvmel
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby))
  (general-def 'insert inf-ruby-mode-map
    "C-k" 'comint-next-input
    "C-l" 'comint-previous-input
    "C-;" 'comint-send-input))

;; (use-package yari
;;   :ensure t
;;   :interpreter "ruby")

(use-package rubocop
  :ensure t
  :interpreter "ruby")

;; Python
;; https://jonathanabennett.github.io/blog/2019/06/20/python-and-emacs-pt.-1/
(use-package elpy
  :ensure t
  :init
  (setq python-indent-offset 4)
  (elpy-enable))

;; SQL
(use-package sqlup-mode
  :ensure t
  :config
  (add-to-list 'sqlup-blacklist "name")

  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  (add-hook 'sql-mode-hook 'sqlup-mode))
(use-package sql-indent
  :ensure t
  :config (add-hook 'sql-mode-hook 'sqlind-minor-mode))


;;; macOS SPECIFIC

;; set the path variable (important for macOS?)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Allow hash to be entered on UK macbook keyboard layout
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))
