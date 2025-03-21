(setq lsp-clients-clangd-executable "/usr/bin/clangd"
      lsp-clients-clangd-args
      '("--background-index"
        "--pch-storage=memory"
        "--clang-tidy"
        "--suggest-missing-includes"
        "--header-insertion=iwyu"
        "--index-file=global"
        "--compile-commands-dir=build"))

(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-ts-mode))
(use-package flycheck-clang-tidy
  :ensure t
  :after flycheck
  :config
  (flycheck-clang-tidy-setup))

;; TODO: setup a dir for compile-commands (read from file-specific conf file for the project)
;; TODO: my/get-clangd-compile-commands-die
;; (setq lsp-clients-clangd-args
;;       (append lsp-clients-clangd-args
;;               (list (concat "--compile-commands-dir=" (my/get-clangd-compile-commands-dir))))))


;; TODO: define installed lsp-clients
;; (setq lsp-clients-python-library-directories '("~/.local/share/nvim/mason/bin/"))
;; (setq lsp-clients-jdtls-executable "~/.local/share/nvim/mason/bin/jdtls")

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)))  ;; or (lsp-deferred)
  :config
  (setq lsp-pyright-venv-path "~/src/pyenv/"
        lsp-pyright-venv-directory "global"
        lsp-pyright-python-executable-cmd "~/src/pyenv/global/bin/python3"
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-typechecking-mode "basic"))

(use-package apheleia
  :ensure t
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(isort black))

  (setf (alist-get 'black apheleia-formatters)
        '("~/.config/emacs/formatters/black-wrapper"))
  (setf (alist-get 'isort apheleia-formatters)
        '("~/.config/emacs/formatters/isort-wrapper"))

  (apheleia-global-mode +1))
