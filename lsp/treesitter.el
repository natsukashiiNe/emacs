(setq treesit-language-source-alist
      '((bash        "https://github.com/tree-sitter/tree-sitter-bash")
        (c           "https://github.com/tree-sitter/tree-sitter-c")
        (cpp         "https://github.com/tree-sitter/tree-sitter-cpp")
        (css         "https://github.com/tree-sitter/tree-sitter-css")
        (go          "https://github.com/tree-sitter/tree-sitter-go")
        (html        "https://github.com/tree-sitter/tree-sitter-html")
        (javascript  "https://github.com/tree-sitter/tree-sitter-javascript" "master" "tree-sitter-javascript/src")
        (json        "https://github.com/tree-sitter/tree-sitter-json")
        (lua         "https://github.com/Azganoth/tree-sitter-lua")
        (make        "https://github.com/alemuller/tree-sitter-make")
        (markdown    "https://github.com/ikatyang/tree-sitter-markdown")
        (python      "https://github.com/tree-sitter/tree-sitter-python")
        (rust        "https://github.com/tree-sitter/tree-sitter-rust")
        (toml        "https://github.com/tree-sitter/tree-sitter-toml")
        (typescript  "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx         "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (yaml        "https://github.com/ikatyang/tree-sitter-yaml")))

(defun my/treesit-install-all-languages ()
  "Install all Tree-sitter languages listed in `treesit-language-source-alist`."
  (interactive)
  (dolist (lang (mapcar #'car treesit-language-source-alist))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (python-mode . python-ts-mode)
        ;; and so on...
        ))

(use-package treesit-fold
  :straight (treesit-fold
             :type git
             :host github
             :repo "emacs-tree-sitter/treesit-fold")
  :hook ((c-ts-mode . treesit-fold-mode)
         (c++-ts-mode . treesit-fold-mode)))

