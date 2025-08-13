(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil          ; Enable documentation on hover
        lsp-ui-doc-position 'at-point ; Show docs at point
        lsp-ui-sideline-enable nil     ; Enable sideline diagnostics
        lsp-ui-peek-enable nil
        lsp-ui-sideline-show-code-actions nil; Enable peek definitions
        lsp-auto-execute-action nil))


(setq lsp-ui-sideline-enable t
      lsp-ui-sideline-show-diagnostics t
      lsp-ui-sideline-show-code-actions t     ;; or t, if you want actions there
      lsp-ui-sideline-diagnostic-max-lines 3
      lsp-ui-sideline-delay 0)
