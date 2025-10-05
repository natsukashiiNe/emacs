;;; lsp-config.el --- LSP Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified LSP configuration for all languages

;; Disable alive-lsp before lsp-mode loads
(with-eval-after-load 'lsp-mode
  (setq lsp-disabled-clients '(alive-lsp)))

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

  (lsp-clients-common-lisp-executable "~/.roswell/bin/cl-lsp")
  (lsp-common-lisp-load-pathes '("/home/nane/quicklisp/"))

  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (c++-ts-mode . lsp)
         (java-mode . lsp)
         (python-mode . lsp)
         ;; lisp-mode is handled in lisp-dev-setup.el
         (prog-mode . (lambda ()
                        (unless (or (derived-mode-p 'emacs-lisp-mode)
                                    (derived-mode-p 'lisp-mode))
                          (lsp))))))


(provide 'lsp-config)
;;; lsp-config.el ends here
