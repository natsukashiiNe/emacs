(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
(setenv "PKG_CONFIG_PATH"
        (concat "/usr/local/lib/pkgconfig" ":" (getenv "PKG_CONFIG_PATH")))
(add-to-list 'exec-path "/usr/local/bin")

(use-package pyvenv
  :config
  (pyvenv-activate "~/src/pyenv/global")
  (setq python-shell-interpreter "python"))


(use-package sudo-edit)
