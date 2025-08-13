(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-auto-configure t
        lsp-enable-snippet nil
        lsp-session-file "~/.emacs.d/lsp-session-v1"
        lsp-prefer-flymake nil
        lsp-idle-delay 0.2
        lsp-log-io nil
        lsp-enable-symbol-highlighting t
        lsp-semantic-tokens-apply-modifiers t)

  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (java-mode . lsp)
         (python-mode . lsp)
         (prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode)
                          (lsp)))))  ;; Disable LSP for Emacs Lisp

  ;;:hook ((c-mode  . lsp-deferred)
  ;;       (c++-mode . lsp-deferred))

  :config
  (setq lsp-auto-guess-root t)
  (setq xref-search-program 'ripgrep)
  (setq xref-backend-functions '(lsp--xref-backend)))

