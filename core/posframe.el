;;; posframe-setup.el --- Posframe settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Setup of the posframe plugins: settings, supportive plugina, its respective hotkeys.

;;; Code:

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
         (eq this-command 'projectile-grep)
         (eq this-command 'project-find-regexp)
         
         (eq this-command 'lsp-find-references)
         (eq this-command 'lsp-find-definition)
         (eq this-command 'xref-find-references)

         ;; Project navigation
         (eq this-command 'consult-fd)
         (eq this-command 'consult-grep)
         (eq this-command 'consult-ripgrep)
         (eq this-command 'consult-project-buffer)
         
         (eq this-command 'consult-xref)
         (eq this-command 'consult-lsp-diagnostics)
         (eq this-command 'consult-lsp-file-symbols)

         ;; help buffers
         (eq this-command 'consult-info)

         ;; Buffer navigation
         (eq this-command 'consult-line)
         (eq this-command 'consult-imenu)
         (eq this-command 'consult-outline)
         (eq this-command 'consult-flycheck)
         (eq this-command 'consult-flyspell)
         (eq this-command 'consult-org-heading)
         (eq this-command 'consult-lsp-symbols)
         (eq this-command 'consult-lsp-file-symbols)

         (eq this-command 'evil-collection-consult-mark)
         (eq this-command 'evil-collection-consult-jump-list)
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

(provide posframe-settings)
;;; posframe-settings.el ends here
