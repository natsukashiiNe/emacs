(use-package posframe
  :ensure t)

(use-package vertico-posframe
  :ensure t
  :after vertico
  :custom
  ;; Position handler
  (vertico-posframe-poshandler 'posframe-poshandler-frame-center)
  (vertico-posframe-border-width 10)
  ;; Optional extra posframe parameters
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8)
     ))
  :config
  (vertico-posframe-mode 1))

(defun my/disable-vertico-posframe ()
  "Disable vertico-posframe for certain commands."
  (when (or 
         (eq this-command 'consult-line)
         (eq this-command 'projectile-grep)
         (eq this-command 'lsp-find-references)
         (eq this-command 'consult-lsp-diagnostics)
         (eq this-command 'consult-lsp-file-symbols)
         (eq this-command 'consult-xref)
         (eq this-command 'xref-find-references)
         (eq this-command 'consult-ripgrep)
         (eq this-command 'consult-grep)
         (eq this-command 'project-find-regexp)
         (eq this-command 'consult-project-buffer)
         (eq this-command 'consult-imenu)
         (eq this-command 'consult-outline)
         )
    (shell-command "notify-send 'Hello from Emacs'")
    (vertico-posframe-mode -1)
    (setq vertico-count 12)
    ))

(defun my/enable-vertico-posframe ()
  "Re-enable vertico-posframe after minibuffer exits."
  (unless vertico-posframe-mode
    (vertico-posframe-mode 1)
    (setq vertico-count 20)
    ))

(add-hook 'minibuffer-setup-hook #'my/disable-vertico-posframe)

(add-hook 'minibuffer-exit-hook #'my/enable-vertico-posframe)

;; TESTING

(defun my/log-every-command ()
  (message "Command: %s" this-command))

(add-hook 'pre-command-hook #'my/log-every-command)


(remove-hook 'pre-command-hook #'my/log-every-command)
