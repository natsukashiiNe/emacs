;;; lsp-servers.el --- LSP Mode Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Setting up different LSP servers

;;; Code:
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  ;; clangd client options belong to lsp-mode (they're defcustoms there)
  (lsp-clients-clangd-executable "/usr/bin/clangd")
  (lsp-clients-clangd-args
   '("--background-index"
     "--pch-storage=memory"
     "--clang-tidy"
     "--suggest-missing-includes"
     "--header-insertion=iwyu"
     "--index-file=global"
     "--compile-commands-dir=build")))

;; Associate C++ modules with tree-sitter mode
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-ts-mode))

(use-package flycheck-clang-tidy
  :after flycheck
  :commands (flycheck-clang-tidy-setup)
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
  :custom
  (lsp-pyright-venv-path "~/src/pyenv")
  (lsp-pyright-python-executable-cmd "~/src/pyenv/global/bin/python3")
  (lsp-pyright-use-library-code-for-types t)
  (lsp-pyright-typechecking-mode "workspace"))

;; Formatting with Apheleia
(use-package apheleia
  :commands (apheleia-global-mode)
  :init
  ;; Forward-declare to silence “free variable” warnings for alists
  (defvar apheleia-mode-alist)
  (defvar apheleia-formatters)
  :config
  ;; mutate the alists after apheleia is loaded
  (setf (alist-get 'python-mode apheleia-mode-alist) '(isort black))
  (setf (alist-get 'black apheleia-formatters)
        '("~/.config/emacs/formatters/black-wrapper"))
  (setf (alist-get 'isort apheleia-formatters)
        '("~/.config/emacs/formatters/isort-wrapper"))
  (apheleia-global-mode +1))


;; Java (JDT LS)
(use-package lsp-java
  :after lsp-mode
  :hook (java-mode . lsp)
  :custom
  (lsp-java-java-path "/usr/lib/jvm/java-23-openjdk/bin/java")
  (lsp-java-jdt-download-url
   "https://download.eclipse.org/jdtls/milestones/1.11.0/jdt-language-server-latest.tar.gz"))

(provide 'lsp-servers)
;;; lsp-servers.el ends here
