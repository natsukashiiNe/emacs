(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)   ;; Act on the current candidate
   ("C-;" . embark-dwim)) ;; Context-sensitive do-what-I-mean
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult))
