;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.

(setq user-full-name "David Mynors"
      user-mail-address "hello@davidmyno.rs")

(load! "bits/ui")
(load! "bits/git")
(load! "bits/evil")
(load! "bits/basics")
(load! "bits/custom")
(load! "bits/search")
(load! "bits/secrets")
(load! "bits/functions")

(load! "bits/lang/js")
