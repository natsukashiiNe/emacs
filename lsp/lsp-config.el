(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l"      ;; Prefix for LSP commands
        lsp-auto-configure t           ;; Auto-detect root directory
        lsp-enable-snippet nil         ;; Disable snippet support
        lsp-session-file "~/.emacs.d/lsp-session-v1" ;; Store session
        lsp-prefer-flymake t           ;; Use `flycheck` instead of `flymake`
        lsp-idle-delay 0.2             ;; Reduce delay for better responsiveness
        lsp-log-io nil                 ;; Disable verbose logging unless debugging
        lsp-enable-symbol-highlighting t) ;; Highlight symbol occurrences

  :hook ((prog-mode . lsp)
         (c-ts-mode . lsp)
         (c++-ts-mode . lsp)
         (java-mode . lsp)
         (python-mode . lsp))
  
  :config
  (setq lsp-auto-guess-root t)  ;; Ensure LSP picks the correct root directory
  (setq xref-search-program 'ripgrep) ;; Use ripgrep for faster searches
  (setq xref-backend-functions '(lsp--xref-backend))) ;; Ensure xref uses LSP
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
        lsp-ui-peek-enable t))       ; Enable peek definitions
