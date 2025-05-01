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


;; CUSTOM TREE-SITTER
(tree-sitter-hl-add-patterns 'c 
  [ ;; Highlight object-like macro names (no params)
   (preproc_def name: (identifier) @function.special)
   ;; Highlight function-like macro names
   (preproc_function_def name: (identifier) @function.special)
   ;; Highlight parameters in function-like macros
   (preproc_function_def 
    parameters: (preproc_params (identifier) @variable.parameter))
   ])

(tree-sitter-hl-add-patterns 'cpp 
  [ ;; Same patterns for C++ mode
   (preproc_def name: (identifier) @function.special)
   (preproc_function_def name: (identifier) @function.special)
   (preproc_function_def 
    parameters: (preproc_params (identifier) @variable.parameter))
   ])
