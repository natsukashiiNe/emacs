(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-modal-icon t))

