(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/_progs" "~/_projects"))
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
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
  (add-hook 'emacs-startup-hook
            (lambda ()
              (persp-state-load persp-state-default-file)))
  
  (when (daemonp)
    (add-hook 'server-after-make-frame-hook
              (lambda ()
                (persp-state-load persp-state-default-file))))
  
  (add-hook 'kill-emacs-hook
            (lambda ()
              (persp-state-save persp-state-default-file)))
  
  (add-hook 'delete-frame-functions
            (lambda (_frame)
              (when (<= (length (frame-list)) 1)
                (persp-state-save persp-state-default-file)))))



(use-package eyebrowse
  :init
  (eyebrowse-mode t)
  :config
  (setq eyebrowse-new-workspace t)  ;; Always create new empty workspaces
  (setq eyebrowse-wrap-around t)    ;; Cycle when reaching the end
  (setq eyebrowse-mode-line-separator " | ")
  (setq eyebrowse-mode-line-style 'always)
  (setq eyebrowse-keymap-prefix (kbd "C-c w"))) ;; TODO

;; Keybindings for Eyebrowse under C-n
;; (keymap-set my-tmux-map "k" #'eyebrowse-prev-window-config)
;; (keymap-set my-tmux-map "j" #'eyebrowse-next-window-config)
;; (keymap-set my-tmux-map "e" #'eyebrowse-last-window-config)
;; (keymap-set my-tmux-map "r" #'eyebrowse-rename-window-config)
