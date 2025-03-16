(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-S-j" . vertico-last)
              ("C-S-k" . vertico-first)
              ("M-j" . vertico-scroll-down)
              ("M-k" . vertico-scroll-up))
  :config
  (setq vertico-count 20
        vertico-resize t))
