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
