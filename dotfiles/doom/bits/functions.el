;;; bits/functions.el -*- lexical-binding: t; -*-

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

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›” with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2020-10-17"
  (interactive)
  (let* (
	 ($inputStr
	  (if (use-region-p)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (let ($p0 $p1 $p2
		      ;; chars that are likely to be delimiters of file path or url, e.g. whitespace, comma. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
		      ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
	      (setq $p0 (point))
	      (skip-chars-backward $pathStops)
	      (setq $p1 (point))
	      (goto-char $p0)
	      (skip-chars-forward $pathStops)
	      (setq $p2 (point))
	      (goto-char $p0)
	      (buffer-substring-no-properties $p1 $p2))))
	 ($path
	  (replace-regexp-in-string
	   "^file:///" "/"
	   (replace-regexp-in-string
	    ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
	(if (fboundp 'xahsite-url-to-filepath)
	    (let (($x (xahsite-url-to-filepath $path)))
	      (if (string-match "^http" $x )
		  (browse-url $x)
		(find-file $x)))
	  (progn (browse-url $path)))
      (progn ; not starting “http://”
	(if (string-match "#" $path )
	    (let (
		  ( $fpath (substring $path 0 (match-beginning 0)))
		  ( $fractPart (substring $path (1+ (match-beginning 0)))))
	      (if (file-exists-p $fpath)
		  (progn
		    (find-file $fpath)
		    (goto-char (point-min))
		    (search-forward $fractPart ))
		(when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
		  (find-file $fpath))))
	  (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
	      (let (
		    ($fpath (match-string 1 $path))
		    ($line-num (string-to-number (match-string 2 $path))))
		(if (file-exists-p $fpath)
		    (progn
		      (find-file $fpath)
		      (goto-char (point-min))
		      (forward-line (1- $line-num)))
		  (when (y-or-n-p (format "file no exist: 「%s」. Create?" $fpath))
		    (find-file $fpath))))
	    (if (file-exists-p $path)
		(progn ; open f.ts instead of f.js
		  (let (($ext (file-name-extension $path))
			($fnamecore (file-name-sans-extension $path)))
		    (if (and (string-equal $ext "js")
			     (file-exists-p (concat $fnamecore ".ts")))
			(find-file (concat $fnamecore ".ts"))
		      (find-file $path))))
	      (if (file-exists-p (concat $path ".el"))
		  (find-file (concat $path ".el"))
		(message "nothing to open")))))))))

(use-package! edit-indirect
  :after-call md/narrow-dwim)

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
