;; FUNTION TO HOOK ON GUI FRAME

;; (use-package which-key
;;   :demand t
;;   :config
;;   (which-key-mode 1) ;; Enable Which-Key globally
;;   (setq which-key-side-window-location 'bottom
;;         which-key-sort-order #'which-key-key-order-alpha
;;         which-key-allow-imprecise-window-fit nil
;;         which-key-sort-uppercase-first nil
;;         which-key-add-column-padding 1
;;         which-key-max-display-columns nil
;;         which-key-min-display-lines 10
;;         which-key-side-window-slot -10
;;         which-key-side-window-max-height 0.25
;;         which-key-idle-delay 0.01
;;         which-key-max-description-length 50
;;         which-key-separator " → "))


(use-package which-key
  :demand t
  :config
  (which-key-mode 1))

(with-eval-after-load 'which-key
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-allow-imprecise-window-fit nil
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 4
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.15
        which-key-idle-delay 0.01
        which-key-max-description-length 50
        which-key-separator " → ")
  ;; force which-key to refresh its UI
  (which-key-mode -1)
  (which-key-mode 1)
  (message "loading settings for which-key!"))

