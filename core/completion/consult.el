(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line))
  :config
  (setq consult-project-root-function #'vc-root-dir
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))


(use-package consult-lsp)
(use-package consult-flycheck)
(use-package consult-flyspell)
