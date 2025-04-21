(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :hook (lua-mode . (lambda () (when (featurep 'tree-sitter) (tree-sitter-mode) (tree-sitter-hl-mode))))
  :config
  (setq lua-indent-level 4))  ;; Set your preferred indentation level

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :hook (rust-mode . (lambda () (when (featurep 'tree-sitter) (tree-sitter-mode) (tree-sitter-hl-mode)))))
