(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-auto-configure t
        lsp-enable-snippet nil
        lsp-session-file "~/.emacs.d/lsp-session-v1"
        lsp-prefer-flymake t
        lsp-idle-delay 0.2
        lsp-log-io nil
        lsp-enable-symbol-highlighting t
        ;; lsp-semantic-tokens-enable t
        )

  :hook ((c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (java-mode . lsp)
         (python-mode . lsp)
         (prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode)
                          (lsp)))))  ;; Disable LSP for Emacs Lisp
  
  :config
  (setq lsp-auto-guess-root t)
  (setq xref-search-program 'ripgrep)
  (setq xref-backend-functions '(lsp--xref-backend)))

;; Ensure flymake is enabled for LSP buffers
(add-hook 'lsp-mode-hook #'flymake-mode)



;; TODO: inline diagnostics


(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t          ; Enable documentation on hover
        lsp-ui-doc-position 'at-point ; Show docs at point
        lsp-ui-sideline-enable t     ; Enable sideline diagnostics
        lsp-ui-peek-enable t
        lsp-ui-sideline-show-code-actions t))       ; Enable peek definitions

