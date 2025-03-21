(use-package tree-sitter
  :ensure t
  :hook ((c-mode         . tree-sitter-mode)
         (c++-mode       . tree-sitter-mode)
         (python-mode    . tree-sitter-mode)
         (javascript-mode . tree-sitter-mode))
  :config
  ;; Enable highlighting once tree-sitter is on:
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)
