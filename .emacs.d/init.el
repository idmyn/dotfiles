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

;; http://irreal.org/blog/?p=5916
(setq use-package-enable-imenu-support t)

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; straight.el
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
  (add-hook 'vterm-mode-hook 'input-mono)
  (add-hook 'shell-mode-hook 'input-mono)
  (add-hook 'eshell-mode-hook 'input-mono)
  (add-hook 'ranger-mode-hook 'input-mono))

(setq inhibit-startup-screen t)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Prefer vertical split
;; (setq split-height-threshold nil)
;; (setq split-width-threshold 0)

(display-time-mode 1)
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
 '(default ((t (:inherit nil :stipple nil :background "#fffff8" :foreground "#111111" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width condensed :foundry "nil" :family "Input Sans Narrow"))))
 '(js2-error ((t nil)))
 '(js2-warning ((t nil)))
 '(line-number ((t (:inherit (shadow default) :family "Input Mono Narrow"))))
 '(org-block-begin-line ((t (:height 0.8))))
 '(vterm-color-black ((t (:inherit term-color-black :background "dark gray"))))
 '(web-mode-doctype-face ((t nil)))
 '(web-mode-html-attr-name-face ((t nil)))
 '(web-mode-html-tag-face ((t nil)))
 '(web-mode-json-key-face ((t nil)))
 '(web-mode-symbol-face ((t nil)))
 '(whitespace-tab ((t (:foreground "#9e9e9e")))))
(setq whitespace-display-mappings
      '((tab-mark 9 [124 9] [92 9]))) ;; use pipe char to indicate tab

(global-whitespace-mode t)
(defun my-inhibit-global-whitespace-mode () ;; https://stackoverflow.com/a/6839968
  "Counter-act `global-whitespace-mode'."
  (add-hook 'after-change-major-mode-hook
            (lambda () (whitespace-mode 0))
            :append :local))

;; while we're at it...
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Indentation
(use-package aggressive-indent
  :ensure t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq backward-delete-char-untabify-method nil)
(setq-default electric-indent-inhibit nil)

(custom-set-variables
 '(emojify-display-style (quote image))
 '(emojify-download-emojis-p (quote ask))
 '(emojify-emoji-set "twemoji-v2-22")
 '(emojify-emoji-styles (quote (unicode)))
 '(global-emojify-mode t)
 '(js2-mode-show-parse-errors nil)
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
(use-package simpleclip
  :config
  (require 'simpleclip)
  (defun my-vterm-yank-from-simpleclip ()
    (interactive)
    (kill-new (simpleclip-get-contents))
    (vterm-yank))
  (simpleclip-mode 1))

(defun crux-rename-file-and-buffer () ; https://github.com/bbatsov/crux
  "Rename current buffer and if the buffer is visiting a file, rename it too."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (rename-buffer (read-from-minibuffer "New name: " (buffer-name)))
      (let* ((new-name (read-from-minibuffer "New name: " filename))
             (containing-dir (file-name-directory new-name)))
        (make-directory containing-dir t)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;; Flatiron School niceties
(defun learn-tests ()
  "Run learn tests asynchronously."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
  (async-shell-command "learn")))
(defun learn--f-f ()
  "Run learn --f-f asynchronously."
  (interactive)
  (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
  (async-shell-command "learn --f-f")))

(defun xah-run-current-go-file ()
  "Run or build current golang file.

To build, call `universal-argument' first.

Version 2018-10-12"
  (interactive)
  (when (not (buffer-file-name)) (save-buffer))
  (when (buffer-modified-p) (save-buffer))
  (let* (
         ($outputb "*xah-run output*")
         (resize-mini-windows nil)
         ($fname (buffer-file-name))
         ($fSuffix (file-name-extension $fname))
         ($prog-name "go")
         $cmd-str)
    (setq $cmd-str (concat $prog-name " \""   $fname "\" &"))
    (if current-prefix-arg
        (progn
          (setq $cmd-str (format "%s build \"%s\" " $prog-name $fname)))
      (progn
        (setq $cmd-str (format "%s run \"%s\" &" $prog-name $fname))))
    (progn
      (message "running %s" $fname)
      (message "%s" $cmd-str)
      (shell-command $cmd-str $outputb )
      ;;
      )))

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

(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))
(setq initial-major-mode (quote restclient-mode))
(setq initial-buffer-choice 'xah-new-empty-buffer)
(setq initial-scratch-message "")

;; Emojis
(use-package emojify
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))


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

    "M-u" 'move-border-left
    "M-o" 'move-border-down
    "M-i" 'move-border-up
    "M-p" 'move-border-right

    "s-=" 'text-scale-increase
    "s--" 'text-scale-decrease

    "s-n" 'xah-new-empty-buffer
    "s-i" 'complete-symbol
    "s-r" 'browser-refresh

    "M-SPC" 'ivy-yasnippet)

    (general-define-key (kbd "<C-return>") 'xah-run-current-file)

  (general-create-definer global-leader
    :prefix "SPC")
  (global-leader 'motion 'override
    ;; "f" 'swiper
    ;; "x" 'counsel-M-x
    "f" 'switch-to-buffer
    "d" 'dumb-jump-go
    "b" 'dumb-jump-back
    "n" 'deer
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
    "L" 'learn-tests
    "l" 'learn--f-f
    "a" 'async-shell-command
    "u" 'undo-tree-visualize
    "t" 'vterm-toggle
    "p" 'neotree-project-dir
    "m" 'ivy-imenu-anywhere
    "R" 'crux-rename-file-and-buffer
    "r" 'query-replace)

  (general-create-definer local-leader
    :prefix "m")
    ;; "l" for lookup, "b" for breakpoint, "d" for debug, "e" for evaluate

  (general-def 'normal
    "s" 'avy-goto-word-1
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
    ":" 'evil-forward-char

    "h" 'evil-paste-after
    "H" 'evil-paste-before
    ;; "p" 'evil-ex
    "p" 'projectile-command-map

    "RET" 'other-window

    "C-e" 'er/expand-region

    ;; multiple-cursors
    "C-k" 'evil-multiedit-match-symbol-and-next
    "C-l" 'evil-multiedit-match-symbol-and-prev
    "C-;" 'evil-multiedit-match-all
    ;; "I" 'evil-mc-make-cursor-in-visual-selection-beg
    ;; "A" 'evil-mc-make-cursor-in-visual-selection-end

    ;; ")" 'evil-beginning-of-line

    ;; easier motion around lines and paragraphs
    "J" 'evil-first-non-blank
    "K" 'forward-paragraph
    "L" 'backward-paragraph
    ";" 'evil-last-non-blank)

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
    :config
    ;; (setq undo-tree-auto-save-history t)
    ;; (setq undo-tree-history-directory-alist '(("." . "~/.saves/")))
    ;; (use-package undohist
    ;;   :ensure t
    ;;   :config
    ;;   (undohist-initialize)
    ;;   (setq undohist-ignored-files (list "COMMIT_EDITMSG")))
    (global-undo-tree-mode))
  (use-package expand-region
    :ensure t)

  :config
  (evil-mode 1)
  (setq-default evil-shift-width 2)

  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode 1)))

;; Multiple cursors
(use-package evil-multiedit
  :ensure t)

;; Ivy
(use-package ivy
  :ensure t
  :init ;; use flx if ivy--regex-fuzzy
  (use-package flx
    :ensure t)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (global-set-key "\C-s" 'swiper)

  (use-package counsel
    :ensure t
    :config (counsel-mode 1))

  (general-def
    :keymaps '(ivy-mode-map swiper-map counsel-ag-map)
    "C-j" (kbd "DEL")
    "C-k" 'ivy-next-line
    "C-l" 'ivy-previous-line
    "C-;" 'ivy-alt-done

    "M-k" 'ivy-next-line-and-call
    "M-l" 'ivy-previous-line-and-call
    ))

(use-package ivy-posframe
  :ensure t
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
  (ivy-posframe-mode 1))

(use-package prescient
  :ensure t
  :config
  (use-package ivy-prescient
    :ensure t)
  (use-package company-prescient
    :ensure t)

  (ivy-prescient-mode)
  ;; (company-prescient-mode)
  (prescient-persist-mode))

;; imenu-anywhere
(use-package imenu-anywhere
  :ensure t)

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
                                    :test-dir "test/"
                                    :test-suffix "Test")

  (projectile-register-project-type 'python '("RPGtodo.py"))

  (projectile-register-project-type 'jekyll '(".jekyll-metadata"))

  (projectile-mode +1))

;; Neotree
(use-package neotree
  :ensure t
  :config
  ;; (setq projectile-switch-project-action 'neotree-projectile-action)
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
  (setq neo-theme 'nerd))

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
  ;; (global-company-mode nil)
  )
(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

;; Hippie expand
(general-define-key
  "M-/" 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

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
    ":" 'ranger-find-file))

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
  ;; :hook ((js2-mode) . lsp)
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
   lsp-ui-imenu-enable nil
   lsp-enable-completion-at-point t
   ))


;;; LANGUAGE/MODE SPECIFIC

;; Git
(use-package magit
  :ensure t
  :config
  (with-eval-after-load 'evil
    (add-to-list 'evil-insert-state-modes 'magit-status-mode)
    (add-hook 'git-commit-mode-hook 'evil-insert-state)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)))
(global-auto-revert-mode t) ; buffers should change when branch changes

;; libvterm
(add-to-list 'load-path "~/.emacs.d/lisp/emacs-libvterm")
(use-package vterm
  :config
  (require 'vterm-toggle)
  (define-key vterm-mode-map (kbd "<escape>") 'evil-escape) ;; couldn't get general to work here
  (general-def 'motion vterm-mode-map
    "h" 'vterm-yank)
  (general-def 'insert vterm-mode-map
    "s-v" 'my-vterm-yank-from-simpleclip
    "C-k" 'vterm-send-down
    "C-l" 'vterm-send-up
    "C-u" 'vterm--self-insert
    )
  ;; fix paste via simpleclip?
  (add-hook 'vterm-mode-hook 'my-inhibit-global-whitespace-mode))

;; Eshell
; good inspo: https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(setenv "PAGER" "cat")
(eval-after-load 'eshell ; https://www.emacswiki.org/emacs/EshellAutojump
  '(require 'eshell-autojump nil t))
(setq eshell-last-dir-ring-size 500)
(defun eshell-new() ; https://www.emacswiki.org/emacs/EshellMultipleEshellBuffers
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))
(defun eshell-setup-keys() ; implementation inspired by evil-collection
  "Set up `evil' bindings for `eshell'."
  (general-def eshell-mode-map
    "s-n" 'eshell-new)
  (define-key eshell-mode-map (kbd "<s-backspace>") 'eshell-kill-input)
  (general-def 'insert eshell-mode-map
    "C-k" 'eshell-next-matching-input-from-input
    "C-l" 'eshell-previous-matching-input-from-input
    ;; "C-;" 'eshell-send-input
    ))
(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))
(add-hook 'eshell-first-time-mode-hook 'eshell-setup-keys)
(add-hook 'eshell-mode-hook (lambda ()
                              (eshell/alias "e" "find-file $1")
                              (eshell/alias "ff" "find-file $1")
                              (eshell/alias "emacs" "find-file $1")
                              (eshell/alias "ee" "find-file-other-window $1")

                              (eshell/alias "la" "ls -A")
                              (eshell/alias "ll" "ls -hopA")

                              (eshell/alias "gd" "magit-diff-unstaged")
                              (eshell/alias "gds" "magit-diff-staged")
                              (eshell/alias "d" "dired $1")

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)))
                              ))
(add-hook 'eshell-directory-change-hook (lambda ()
                                          (concat (eshell/ls) " -A")))

(defun tidy-learn-buffer ()
  (interactive)
  (flush-lines "^[[:space:]]*# "))

(defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output

(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))
(defun with-face (str &rest face-plist) ; https://www.emacswiki.org/emacs/EshellPrompt#toc3
  (propertize str 'face face-plist))
(defun git-prompt-branch-name () ; https://superuser.com/a/1265169
  "Get current git branch name"
  (let ((args '("symbolic-ref" "HEAD" "--short")))
    (with-temp-buffer
      (apply #'process-file "git" nil (list t nil) nil args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(setq eshell-prompt-function
      (lambda nil
        (let ((branch-name (git-prompt-branch-name)))
          (concat
           "\n" (pwd-replace-home(eshell/pwd)) "\n "
           (if branch-name (with-face branch-name :foreground "gray") )
           " $ "))))

(use-package load-bash-alias
  :ensure t
  :config
  (setq load-bash-alias-bashrc-file "~/.aliases"))

(setq eshell-history-size 1000000)
(setq shell-file-name "bash") ; for cases where I can't use eshell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(general-def 'insert shell-mode-map
    "C-k" 'comint-next-input
    "C-l" 'comint-previous-input
    ;; "C-;" 'comint-send-input
    )
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
  ;; :ensure t
  :config
  (general-def 'web-mode-map
    "M-;" nil)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'emmet-mode) ; auto-start on any markup modes
  (add-hook 'sgml-mode-hook 'emmet-mode) ; auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ; enable Emmet's css abbreviation.
  (general-def 'insert web-mode-map
    "C-SPC" 'emmet-expand-line))
(use-package evil-matchit
  :ensure t
  :config (global-evil-matchit-mode 1))

(use-package browser-refresh
  :config
  (setq browser-refresh-default-browser 'brave)
  (setq browser-refresh-save-buffer nil)
  (setq browser-refresh-activate nil))

;; Sass
(use-package sass-mode
  ;; https://github.com/nex3/sass-mode
  :init
  (use-package haml-mode
    :ensure t))

;; Javascript
(use-package js2-mode
  :ensure t
  :config
  (setq-default js2-basic-offset 2)
  ;; (setq js2-mode-show-parse-errors t)
  (setq js2-strict-missing-semi-warning nil)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  ;; (flycheck-add-mode 'javascript-standard 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (use-package rjsx-mode
    :ensure t))
(use-package json-mode
  :ensure t)

(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-args '(
                           "--no-semi" "false"
                           )))

(use-package eglot
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'eglot-ensure)
  (add-hook 'js2-mode-hook 'eglot-ensure))


;; Ruby
(use-package robe
  :ensure t
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  ;; (add-hook 'ruby-mode-hook 'robe-mode)
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
;; (add-hook 'find-file-hook 'open-rb-hook)
;; (defun open-rb-hook ()
;;   (when (string= (file-name-extension buffer-file-name) "rb")
;;     (robe-start)))

(use-package rspec-mode
  :ensure t
  :config
  (setq rspec-use-rvm t)
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))

  (ad-activate 'rspec-compile))

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

(use-package rubocop
  :ensure t
  :interpreter "ruby")

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; Python
;; https://jonathanabennett.github.io/blog/2019/06/20/python-and-emacs-pt.-1/
(use-package elpy
  :ensure t
  :init
  (setq python-indent-offset 4)
  (elpy-enable)
  :config
  (when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)))

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

;; Go
(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode 'gofmt-before-save))
(add-to-list 'load-path "~/go/src/golang.org/x/lint/misc/emacs/" t)
(require 'golint)

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

;; HTTP
(use-package restclient
  :ensure t
  :config
  ; https://github.com/pashky/restclient.el/issues/212#issuecomment-515759772
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))


;;; macOS SPECIFIC

;; set the path variable (important for macOS?)
(use-package exec-path-from-shell
     :ensure t
     :defer 0.1
     :config
     (exec-path-from-shell-initialize))

;; Allow hash to be entered on UK macbook keyboard layout
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; I've put this at the end because something else in this file was overriding it
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))

;; (setq explicit-shell-file-name "/bin/bash")
;; (setenv "SHELL" shell-file-name)
;; (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
