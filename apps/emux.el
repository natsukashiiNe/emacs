(defun elmux-list-servers ()
  "Return a list of Emacs servers in `server-socket-dir`."
  (let ((dir (or server-socket-dir "/run/user/1000/emacs")))
    (seq-filter
     (lambda (name)
       ;; Exclude . and .. and any leftover lock files
       (not (string-match-p "^\\." name)))
     (directory-files dir nil "^[^.].*"))))

(defun elmux--server-alive-p (server-name)
  "Check if SERVER-NAME is alive using `emacsclient --eval` with a 2s timeout."
  (with-timeout (2 nil)
    (eq 0 (call-process "emacsclient" nil nil nil
                        "-s" server-name
                        "--eval" "(+ 1 2)"
                        "--timeout=2"))))

(defun elmux-list-servers-alive ()
  "Return a list of servers that respond quickly to `--eval`."
  (seq-filter #'elmux--server-alive-p (elmux-list-servers)))

(defun elmux-switch-session ()
  "Prompt for a running server to connect to, open a client frame, and kill this Emacs."
  (interactive)
  (let* ((servers (elmux-list-servers))
         (choice (completing-read "Switch to daemon: " servers nil t)))
    (when (and choice (not (string-empty-p choice)))
      ;; Start a new Emacsclient frame for that server
      (start-process "elmux-switch" nil "emacsclient" "-s" choice "-c")
      )))


(let* ((choice (completing-read
                "Choose Emacs daemon: "
                (elmux-list-servers) nil t)))
  (start-process
   "emux-switch" 
   nil
   "emacsclient" "-s" choice "-c"))
