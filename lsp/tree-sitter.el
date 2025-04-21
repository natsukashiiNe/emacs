(use-package tree-sitter
  :hook ((c-mode c++-mode lua-mode python-mode js-mode) . tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :hook ((c-mode c++-mode lua-mode python-mode js-mode) . ts-fold-mode)
  :custom
  (ts-fold-line-count-show nil)
  (ts-fold-replacement " ▶ … "))
