;;; clojure.el -*- lexical-binding: t; -*-

;; https://github.com/djwhitt/dotfiles/blob/818df88452125e4f9cb1201ef746782c22d4fc98/doom.d/+clojure.el

;;;###autoload
(defun babashka-scratch ()
  (interactive)
  (let* ((buf (get-buffer-create "*babashka-scratch*")))
    (with-current-buffer buf
      (clojure-mode)
      (babashka-jack-in
       (lambda (_)
         (sesman-link-session 'CIDER '("babashka") 'buffer buf))))
    (switch-to-buffer buf)))

;;;###autoload
(defun babashka-quit ()
  (interactive)
  (setq sesman-links-alist
        (a-dissoc sesman-links-alist '(CIDER . "babashka")))
  (when (get-buffer "*babashka-nrepl-server*")
    (kill-buffer "*babashka-nrepl-server*"))
  (when (get-buffer "*babashka-repl*")
    (kill-buffer "*babashka-repl*")))

;;;###autoload
(defun babashka-jack-in (&optional connected-callback)
  (interactive)
  (babashka-quit)
  (let* ((cmd "bb --nrepl-server 0")
         (serv-buf (get-buffer-create "*babashka-nrepl-server*"))
         (host "127.0.0.1")
         (repl-builder (lambda (port)
                         (lambda (_)
                           (let ((repl-buf (get-buffer-create "*babashka-repl*")))
                             (with-current-buffer repl-buf
                               (cider-repl-create (list :repl-buffer repl-buf
                                                        :repl-type 'clj
                                                        :host host
                                                        :port port
                                                        :project-dir "~"
                                                        :session-name "babashka"
                                                        :repl-init-function (lambda ()
                                                                              (setq-local cljr-suppress-no-project-warning t
                                                                                          cljr-suppress-middleware-warnings t)
                                                                              (rename-buffer "*babashka-repl*")))))))))
         (port-filter (lambda (serv-buf)
                        (lambda (process output)
                          (when (buffer-live-p serv-buf)
                            (with-current-buffer serv-buf
                              (insert output)
                              (when (string-match "Started nREPL server at 127.0.0.1:\\([0-9]+\\)" output)
                                (let ((port (string-to-number (match-string 1 output))))
                                  (setq nrepl-endpoint (list :host host :port port))
                                  (let ((client-proc (nrepl-start-client-process
                                                      host
                                                      port
                                                      process
                                                      (funcall repl-builder port))))
                                    (set-process-query-on-exit-flag client-proc nil)
                                    (when connected-callback
                                      (funcall connected-callback client-proc)))))))))))
    (with-current-buffer serv-buf
      (setq nrepl-is-server t
            nrepl-server-command cmd))
    (let ((serv-proc (start-file-process-shell-command "babashka-nrepl-server" serv-buf cmd)))
      (set-process-query-on-exit-flag serv-proc nil)
      (set-process-filter serv-proc (funcall port-filter serv-buf))
      (set-process-sentinel serv-proc 'nrepl-server-sentinel)
      (set-process-coding-system serv-proc 'utf-8-unix 'utf-8-unix)))
  nil)
