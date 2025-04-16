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
