;;; -*- lexical-binding: t; -*-

(setq user-full-name "David Mynors"
      user-mail-address "hello@davidmyno.rs")

(show-smartparens-global-mode 1)

(setq display-line-numbers-type nil)

;; load secrets (encrypted with git-crypt)
(load "~/.config/nixpkgs/dotfiles/doom/secrets.el")

;; when I open emacs I want it to fill my screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package! eink-theme
  :custom
  (doom-theme 'eink)
  (doom-font (font-spec :family "Iosevka SS09" :size 15))
  (doom-variable-pitch-font (font-spec :family "Iosevka Aile"))
  :config
  (global-hl-line-mode)
  (column-number-mode)
  :custom-face
  (region ((t (:background "peach puff"))))
  (hl-line ((t (:inherit highlight :background "lavender"))))
  (lsp-ui-sideline-code-action ((t (:foreground "limegreen"))))
  (rustic-string-interpolation-face ((t)))
  (terraform--resource-name-face ((t)))
  (terraform--resource-type-face ((t)))
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

(setq current-theme "eink")
(defun toggle-theme ()
  (interactive)
  (if (equal current-theme "eink")
      (progn
        (setq current-theme "gruvbox")
        (load-theme 'gruvbox-dark-soft t)
        (global-hl-line-mode -1)
        (face-spec-set 'region '((t (:foreground "white" :background "slate blue")))))
    (progn
      (setq current-theme "eink")
      (load-theme 'eink t)
      (global-hl-line-mode 1)
      (face-spec-set 'region '((t (:foreground "black" :background "peach puff")))))))

(map!
 :leader
 :prefix "t"
 :desc "Theme" "t" 'toggle-theme)

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

(when (eq system-type 'darwin) (ns-auto-titlebar-mode))

(defun xah-paste-or-paste-previous ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.

When `universal-argument' is called first with a number arg, paste that many times.

URL `http://ergoemacs.org/emacs/emacs_paste_or_paste_previous.html'
Version 2017-07-25"
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (progn
          (dotimes ($i (prefix-numeric-value current-prefix-arg))
            (yank)))
      (if (eq real-last-command this-command)
          (yank-pop 1)
        (yank)))))

(require 'edit-indirect)
(defun md/narrow-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, or
  defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.
  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (edit-indirect-region (region-beginning)
                               (region-end)
                               t))
        (edit-indirect--overlay
         (edit-indirect-commit))
        (org-src-mode
         (org-edit-src-exit))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        ((derived-mode-p 'restclient-mode)
         (restclient-narrow-to-current))
        (t (narrow-to-defun))))
;; Don't ask for confirmation on narrow-to-region
(put 'narrow-to-region 'disabled nil)

(map!
 :leader
 :prefix "t"
 :desc "Narrow" "n" 'md/narrow-dwim)

(map!
 (:after magit
  :map magit-mode-map
  :nv "j" 'magit-section-forward
  :nv "k" 'magit-section-backward))

(map!
 :after evil
 :nvm "H" 'evil-first-non-blank
 :nvm "L" 'evil-last-non-blank)

(map!
 (:after evil
  :map global-map
  :ni "M-h" 'evil-window-left
  :ni "M-j" 'evil-window-down
  :ni "M-k" 'evil-window-up
  :ni "M-l" 'evil-window-right)

 (:after outline
  :map outline-mode-map
  :ni "M-h" 'evil-window-left
  :ni "M-j" 'evil-window-down
  :ni "M-k" 'evil-window-up
  :ni "M-l" 'evil-window-right)

 (:after evil-org
  :map evil-org-mode-map
  :ni "M-h" 'evil-window-left
  :ni "M-j" 'evil-window-down
  :ni "M-k" 'evil-window-up
  :ni "M-l" 'evil-window-right)

 (:after evil-markdown
  :map evil-markdown-mode-map
  :ni "M-h" 'evil-window-left
  :ni "M-j" 'evil-window-down
  :ni "M-k" 'evil-window-up
  :ni "M-l" 'evil-window-right)

 (:after vterm
  :map vterm-mode-map
  :ni "M-h" 'evil-window-left
  :ni "M-j" 'evil-window-down
  :ni "M-k" 'evil-window-up
  :ni "M-l" 'evil-window-right)

 (:after lisp-mode
  :map lispyville-mode-map
  :ni "M-h" 'evil-window-left
  :ni "M-j" 'evil-window-down
  :ni "M-k" 'evil-window-up
  :ni "M-l" 'evil-window-right

  :nv "C-h" 'lispyville-beginning-of-defun
  :nv "C-l" 'lispyville-end-of-defun)

 (:after magit
  :map git-rebase-mode-map
  :ni "M-h" 'evil-window-left
  :ni "M-j" 'evil-window-down
  :ni "M-k" 'evil-window-up
  :ni "M-l" 'evil-window-right))

(map!
 (:after evil
  :i "C-o" '+default/newline-above

  :n "C-j" 'evil-multiedit-match-symbol-and-next
  :n "C-k" 'evil-multiedit-match-symbol-and-prev))

(map!
 (:after evil
  :map global-map
  :i "C-y" 'xah-paste-or-paste-previous
  :i "C-d" 'delete-char
  :i "C-x C-s" 'save-buffer)

 (:after evil-org
  :map evil-org-mode-map
  :i "C-d" 'delete-char))

(map!
 "s--" 'text-scale-decrease
 "s-=" 'text-scale-increase)

(cond
 ((string-equal system-type "darwin")
  (progn
    (map!
     "M-3" (lambda () (interactive) (insert "#"))))))

(map! :map github-review-mode-map
      :when IS-MAC
      :i "M-3" (lambda () (interactive) (insert "#")))

(after! evil
  (setq evil-insert-state-map (make-sparse-keymap)) ; emacs bindings in evil insert state
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-insert-state-message nil)
  (setq +evil-want-o/O-to-continue-comments nil)
  (advice-remove #'newline-and-indent #'+default--newline-indent-and-continue-comments-a))

(map!
 :after evil
 :map evil-surround-mode-map
 :v "S" 'evil-snipe-S)

(after! evil
  (require 'evil-textobj-anyblock)
  (evil-define-text-object my-evil-textobj-anyblock-inner-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object my-evil-textobj-anyblock-a-quote
    (count &optional beg end type)
    "Select the closest outer quote."
    (let ((evil-textobj-anyblock-blocks
           '(("'" . "'")
             ("\"" . "\"")
             ("`" . "`")
             ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (define-key evil-inner-text-objects-map "q" 'my-evil-textobj-anyblock-inner-quote)
  (define-key evil-outer-text-objects-map "q" 'my-evil-textobj-anyblock-a-quote))

(after! ivy-posframe
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-point)))
  (setq ivy-posframe-border-width 0))

(after! ivy
  (setq ivy-re-builders-alist
        '((counsel-imenu . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

(after! ivy
  ;; I don't want to see dired or magit buffers
  (push (lambda (buffer)
          (with-current-buffer buffer
            (-any? #'derived-mode-p '(dired-mode magit-mode)))) ivy-ignore-buffers)

  ;; I don't want to see auxiliary buffers
  (dolist (regexp '("^\\*"))
    (push regexp ivy-ignore-buffers)))

(map!
 :leader
 :prefix "s"
 :desc "Search project" "p" 'deadgrep)
(set-popup-rule! "^\\*deadgrep" :size 0.4)

(after! deadgrep
  (defun deadgrep--project-root ()
    "Guess the project root of the given FILE-PATH."
    (let ((root default-directory)
          (project (projectile-project-root)))
      (when project
        (setq root project))
      (when root
        (deadgrep--lookup-override root)))))

(map! :n "D" 'dash-at-point)

(global-company-mode -1)
(after! company
  (add-to-list 'company-global-modes 'sh-mode t)
  (add-to-list 'company-global-modes 'eshell-mode t)
  (add-to-list 'company-global-modes 'js2-mode t)
  (add-to-list 'company-global-modes 'org-mode t)
  (add-to-list 'company-global-modes 'markdown-mode t))

(setq company-idle-delay 0)

(map! :leader
      :prefix "t"
      :desc "Company" "c" 'company-mode)

(global-eldoc-mode -1)
(remove-hook 'org-mode-hook #'org-eldoc-load)

(use-package eldoc-box
  :after (eldoc company)
  :custom-face
  (eldoc-box-border ((t)))
  (eldoc-box-body ((t (:inherit company-tooltip)))))

(add-hook! 'lsp-eldoc-hook #'lsp-hover #'eldoc-box-hover-mode)
(setq lsp-signature-function 'lsp-signature-posframe)
(after! (company lsp posframe)
  (setq lsp-signature-posframe-params (list :poshandler #'posframe-poshandler-point-window-center
                                            :background-color (face-attribute 'company-tooltip :background)
                                            :height 1
                                            :width 60
                                            :border-width 5
                                            :border-color (face-attribute 'company-tooltip :background)
                                            :min-width 60)))


(map!
 :after evil-org
 :map evil-org-mode-map
 :ni "C-h" 'org-metaleft
 :ni "C-l" 'org-metaright)

(defun eshell-setup-keys() ; implementation inspired by evil-collection
  "Set up `evil' bindings for `eshell'."
  (map!
   :map eshell-mode-map
   :i "C-j" 'eshell-next-matching-input-from-input
   :i "C-k" 'eshell-previous-matching-input-from-input

   :ni "M-h" 'evil-window-left
   :ni "M-j" 'evil-window-down
   :ni "M-k" 'evil-window-up
   :ni "M-l" 'evil-window-right))
(add-hook 'eshell-first-time-mode-hook 'eshell-setup-keys)

(set-eshell-alias!
 "e" "find-file $1"
 "doom" "~/.emacs.d/bin/doom $1")

(setenv "PAGER" "cat")

(after! vterm
  (map!
   :map vterm-mode-map
   :i "C-j" 'vterm-send-down
   :i "C-k" 'vterm-send-up))

(map!
 :leader
 :prefix "f"
 :desc "Browse files" "d" 'dired-jump)

(after! dired
  (map!
   :map dired-mode-map
   :n "h" 'dired-up-directory
   :n "j" 'dired-next-line
   :n "k" 'dired-previous-line
   :n "l" 'dired-find-file

   :n "/" 'dired-narrow
   :n "C-g" 'revert-buffer

   :n "g" 'dired-git-info-mode
   :n "W" 'wdired-change-to-wdired-mode))

(map!
 :leader
 :prefix "t"
 :desc "Olivetti" "o" 'olivetti-mode)
(setq olivetti-body-width 85)
(add-hook 'olivetti-mode-hook (lambda () (markdown-toggle-url-hiding 1)))

(after! js2-mode
  (setq js2-basic-offset 2))

(after! typescript-mode
  (setq typescript-indent-level 2))

(after! (flycheck lsp)
  ;; Add buffer local Flycheck checkers after LSP for different major modes.
  ;; lifted from https://github.com/flycheck/flycheck/issues/1762#issuecomment-749789589
  (defvar-local my-flycheck-local-cache nil)
  (defun my-flycheck-local-checker-get (fn checker property)
    ;; Only check the buffer local cache for the LSP checker, otherwise we get
    ;; infinite loops.
    (if (eq checker 'lsp)
        (or (alist-get property my-flycheck-local-cache)
            (funcall fn checker property))
      (funcall fn checker property)))
  (advice-add 'flycheck-checker-get
              :around 'my-flycheck-local-checker-get)
  (add-hook 'lsp-managed-mode-hook
            (lambda ()
              (when (derived-mode-p 'typescript-tsx-mode)
                (setq my-flycheck-local-cache '((next-checkers . (javascript-eslint))))))))

;(setq flycheck-javascript-eslint-executable "eslint_d")
(setq-hook! 'typescript-tsx-mode-hook flycheck-checker 'javascript-eslint)
(setq lsp-eslint-server-command `("node" ,(expand-file-name (car (last (file-expand-wildcards "~/src/clones/vscode-eslint/server/out/eslintServer.js")))) "--stdio"))
(setq lsp-enable-file-watchers nil)

(setq safe-local-variable-values '((lsp-enabled-clients . (ts-ls eslint))))

(setq css-indent-offset 2)
(add-hook 'css-mode-hook (lambda () (flycheck-mode -1)))

(use-package! web-mode
  :mode "\\.svelte\\'"
  :config (after! flycheck (flycheck-add-mode 'javascript-eslint 'web-mode))
  :custom
  (web-mode-enable-current-element-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-style-padding 0)
  (web-mode-code-indent-offset 2)
  (web-mode-script-padding 0))

(map!
 :after web-mode
 :map web-mode-map
 :i "M-/" 'dabbrev-expand)

(setq grip-binary-path "/usr/local/bin/grip")

(map!
 (:after markdown-mode
  :map evil-markdown-mode-map
  :i "C-d" 'delete-char
  :ni "C-;" 'self-insert-command))

(map!
 (:after sly
  :map sly-mrepl-mode-map
  :i "C-j" 'sly-mrepl-next-input-or-button
  :i "C-k" 'sly-mrepl-previous-input-or-button))

; https://github.com/hlissner/doom-emacs/issues/1530#issuecomment-725588733
(add-hook! 'lsp-after-initialize-hook
  (run-hooks (intern (format "%s-lsp-hook" major-mode))))
(defun go-flycheck-setup ()
  (flycheck-add-next-checker 'lsp 'golangci-lint))
(add-hook 'go-mode-lsp-hook
          #'go-flycheck-setup)

(use-package! just-mode
  :mode "justfile\\'")
