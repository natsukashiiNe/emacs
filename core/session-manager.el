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
  (persp-mode)) ;; Load the session on startup


;; Use lambdas to pass the file path argument
(add-hook 'emacs-startup-hook
          (lambda () (persp-state-load persp-state-default-file)))

(add-hook 'kill-emacs-hook
          (lambda () (persp-state-save persp-state-default-file)))


;; TODO: eyebrowse + tab managment + terminal managments
