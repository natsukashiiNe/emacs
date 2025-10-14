(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-save-repository-buffers nil)
  :config
  (defun my-magit-open-in-new-tab ()
    "Open Magit status in a new tab."
    (interactive)
    (tab-new)
    (magit-status)))

(use-package forge
  :after magit)

(use-package github-review
  :ensure t
  :after forge
  :config
  ;; Use the same token as forge
  (setq github-review-auth-login-marker 'forge)
  
  ;; Bind to Forge for easy access
  (define-key forge-topic-mode-map (kbd "C-c r") 'github-review-forge-pr-at-point)
  
  ;; Optional: view comments inline (not just top-level)
  (setq github-review-view-comments-in-code-lines t)
  (setq github-review-reply-inline-comments t))

(use-package vdiff
  :commands (vdiff-buffers vdiff-files vdiff-receive-changes)
  :config
  ;; Add custom keybindings only when vdiff-mode is active
  (add-hook 'vdiff-mode-hook
            (lambda ()
              (evil-define-key 'normal vdiff-mode-map
                (kbd "J") #'vdiff-next-hunk
                (kbd "K") #'vdiff-previous-hunk)
              ;; Optional Emacs-native bindings (non-evil)
              (keymap-set vdiff-mode-map "S-j" #'vdiff-next-hunk)
              (keymap-set vdiff-mode-map "S-k" #'vdiff-previous-hunk))))

(use-package vdiff-magit
  :after (vdiff magit)
  :config
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit))
