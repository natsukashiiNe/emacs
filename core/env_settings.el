;;; env_settings.el --- Environment and Path Configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Centralized environment configuration for development tools

;;; Code:

;; System paths
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PKG_CONFIG_PATH"
        (concat "/usr/local/lib/pkgconfig" ":" (getenv "PKG_CONFIG_PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(defconst my/home-dir (expand-file-name "~/")
  "User home directory.")

(defconst my/project-dir (expand-file-name "~/_progs/")
  "Default projects directory.")

;; Python environment
(use-package pyvenv
  :straight t
  :config
  (pyvenv-activate "~/src/pyenv/global")
  ;; mode-line
  (setq python-shell-interpreter "python"
        pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode 1))

;; ;; Optional: pyenv-mode if you also use pyenv for version management
;; ;; Comment out if you only use venv
;; (use-package pyenv-mode
;;   :straight t
;;   :if (executable-find "pyenv")
;;   :init
;;   (setq pyenv-mode-map nil)  ; Disable default keybindings
;;   :config
;;   (pyenv-mode 1))

;; Sudo edit utility
(use-package sudo-edit
  :straight t)

                                        ; Development tools paths
(defconst my/lldb-path (executable-find "lldb-vscode")
  "Path to LLDB DAP server.")

(defconst my/clangd-path (or (executable-find "clangd")
                             "/usr/bin/clangd")
  "Path to clangd executable.")

;; Compilation settings
(setq compilation-scroll-output t
      compilation-environment '("TERM=xterm-256color"))

(provide 'env_settings)
;;; env_settings.el ends here
