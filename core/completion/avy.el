(use-package avy
  :straight t
  :config
  ;; Optional tweaks:
  (setq
   avy-keys '(?f ?j ?g ?h ?d ?k ?s ?l ?a ?\; ?w ?o )
   avy-orders-alist '((t . avy-order-closest))
   avy-background nil               ;; dim background during selection
   avy-style 'pre                   ;; how avy overlays hints
   avy-all-windows nil))            ;; search all windows by default

