(use-package treemacs
  :defer t
  :bind
  ("C-x t t" . treemacs)
  :config
  ;; Remember the last project and expand state
  (setq treemacs-persist-file (expand-file-name "treemacs-persist" user-emacs-directory)
        treemacs-is-never-other-window t
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-width 35
        treemacs-show-cursor nil
        treemacs-sorting 'alphabetic-asc)

  ;; Start in current projectile root if not already open
  (defun my/treemacs-smart-toggle ()
    "Open Treemacs in the current Projectile project, or toggle it."
    (interactive)
    (if (treemacs-current-visibility)
        (treemacs)
      (let ((project-root (projectile-project-root)))
        (if project-root
            (treemacs-add-and-display-current-project-exclusively)
          (treemacs)))))
  
  (global-set-key (kbd "C-x t t") #'my/treemacs-smart-toggle)
  )

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package lsp-treemacs
  :after (lsp treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package all-the-icons
  :if (display-graphic-p))
