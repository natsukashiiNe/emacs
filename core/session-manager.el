(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/_progs" "~/_projects"))
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default) ;; or 'ivy, 'helm, etc.
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)

  :custom
  (persp-mode-prefix-key (kbd "C-c n"))
  (persp-suppress-no-prefix-key-warning t)
  (persp-state-default-file "~/.emacs.d/perspective-session")
  
  :init
  (persp-mode)
  
  :config
  ;; Load the session on regular (non-daemon) Emacs startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (persp-state-load persp-state-default-file)))
  
  ;; Load the session in daemon mode (the first time a frame is created)
  ;; Using `server-after-make-frame-hook` ensures it happens once the GUI/terminal frame is ready.
  (when (daemonp)
    (add-hook 'server-after-make-frame-hook
        (lambda ()
            (persp-state-load persp-state-default-file))))
  
  ;; Save the session whenever Emacs is killed (daemon or normal)
  (add-hook 'kill-emacs-hook
    (lambda ()
        (persp-state-save persp-state-default-file)))
  
  ;; Save the session when a client frame is closed **if** it was the last frame
  ;; This ensures that if you do `C-x C-c` in an emacsclient frame,
  ;; the session is saved before disconnecting from the daemon.
  ;; TODO: this does not break child-frames?
  (add-hook 'delete-frame-functions
    (lambda (_frame)
        ;; If this is the last frame, save the session
        (when (<= (length (frame-list)) 1)
        (persp-state-save persp-state-default-file)))))
