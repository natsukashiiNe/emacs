;; (use-package prescient
;;   :straight t
;;   :config
;;   (prescient-persist-mode 1)
;;   (setq prescient-filter-method '(fuzzy)
;;         prescient-sort-full-matches-first t
;;         ;; Configure sorting
;;         prescient-sort-length-enable nil))

;; Don't use orderless as completion style - let prescient handle it
(use-package orderless
  :straight t
  :custom
  (orderless-matching-styles '(orderless-flex))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package vertico-prescient
  :straight t
  :after vertico
  :config
  (vertico-prescient-mode 1)
  ;; ENABLE both filtering and sorting (this was the mistake!)
  (setq vertico-prescient-enable-filtering t
        vertico-prescient-enable-sorting t))
