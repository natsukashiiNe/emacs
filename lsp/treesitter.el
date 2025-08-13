(use-package treesit
  :straight nil ;; built into Emacs 29/30
  :preface
  ;; Prefer TS modes where available
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (c-or-c++-mode   . c-or-c++-ts-mode)
          (python-mode     . python-ts-mode)
          (bash-mode       . bash-ts-mode)
          (json-mode       . json-ts-mode)
          (js-mode         . js-ts-mode)
          (typescript-mode . tsx-ts-mode)  ;; or typescript-ts-mode
          (css-mode        . css-ts-mode)
          (yaml-mode       . yaml-ts-mode)
          (cmake-mode      . cmake-ts-mode)
          (java-mode       . java-ts-mode)
          (rust-mode       . rust-ts-mode)))

  ;; Where compiled grammars (.so/.dylib/.dll) live (keep default + extras)
  (add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter" user-emacs-directory))

  ;; Tune parsing a bit
  (setq treesit-font-lock-level 4
        treesit-inspect-mode-delay 0.15))

;; Automatically install grammars & remap
(use-package treesit-auto
  :straight t
  :commands (treesit-auto-install-all)
  :init
  (setq treesit-auto-install 'prompt)   ;; prompt before downloading grammars
  :config
  (global-treesit-auto-mode)
  ;; Kick once to ensure grammars you care about are present:
  ;; M-x treesit-auto-install-all
  )

;; --- STRUCTURAL JUMPING -------------------------------------------------------
(use-package treesit-jump
  :straight (treesit-jump :type git :host github :repo "dmille56/treesit-jump"))

;; --- FOLDING ------------------------------------------------------------------
(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :hook ((c-ts-mode c++-ts-mode python-ts-mode js-ts-mode tsx-ts-mode
                    rust-ts-mode java-ts-mode bash-ts-mode json-ts-mode css-ts-mode
                    yaml-ts-mode cmake-ts-mode)
         . treesit-fold-mode)
  :bind (("C-c z t" . treesit-fold-toggle)
         ("C-c z a" . treesit-fold-toggle-all)))

