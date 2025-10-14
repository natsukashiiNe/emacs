;; ----------------------------------------------------------------------
;; INIT
;; ----------------------------------------------------------------------

(defun my/set-vertico-count (count)
  "Set =vertico-count' to COUNT."
  (interactive "nNumber of lines: ")
  (setq vertico-count count))

;; ----------------------------------------------------------------------
;; Package
;; ----------------------------------------------------------------------

(use-package vertico
  :init
  (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-S-j" . vertico-last)
              ("C-S-k" . vertico-first)
              ("M-j" . vertico-scroll-down)
              ("M-k" . vertico-scroll-up)
              ("C-S-o" . (lambda ()
                           (interactive)
                           (my/set-vertico-count
                            (+ vertico-count 10))))
              
              ("C-S-w" . (lambda ()
                           (interactive)
                           (my/set-vertico-count
                            (max 1 (- vertico-count 10)))))
              )
  
  :config
  (setq vertico-count 15
        vertico-resize t))

