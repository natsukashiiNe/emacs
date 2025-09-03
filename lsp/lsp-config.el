;;; lsp-config.el --- LSP Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified LSP configuration for all languages

;;; Code:
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-configure t)
  (lsp-enable-snippet nil)
  (lsp-session-file "~/.emacs.d/lsp-session-v1")
  (lsp-prefer-flymake nil)
  (lsp-idle-delay 0.2)
  (lsp-log-io nil)
  (lsp-enable-symbol-highlighting t)
  (lsp-semantic-tokens-apply-modifiers t)
  (lsp-auto-guess-root t)
  (xref-search-program 'ripgrep)
  (xref-backend-functions '(lsp--xref-backend))

  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (java-mode . lsp)
         (python-mode . lsp)
         (prog-mode . (lambda ()
                        (unless (derived-mode-p 'emacs-lisp-mode)
                          (lsp))))))


(provide 'lsp-config)
;;; lsp-config.el ends here
