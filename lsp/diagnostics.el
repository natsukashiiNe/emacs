(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1)
  :config

  (setq flycheck-idle-change-delay 0.1)
  (setq flycheck-display-errors-delay 0.1)
  )

(use-package quick-peek
  :config
  (setq quick-peek-add-spacer nil)
  (set-face-attribute 'quick-peek-background-face nil
                      :background nil
                      :inherit 'default
                      :extend t))


(use-package flycheck-inline
  :after flycheck
  :hook ((flycheck-mode . flycheck-inline-mode))
  :config
  (setq flycheck-inline-display-function
        (lambda (msg pos err)
          (let* ((ov (quick-peek-overlay-ensure-at pos))
                 (contents (quick-peek-overlay-contents ov)))
            (setf (quick-peek-overlay-contents ov)
                  (concat contents (when contents "\n") msg))
            (quick-peek-update ov)))
        flycheck-inline-clear-function #'quick-peek-hide)
  )

;; (use-package flycheck-popup-tip
;;   :after flycheck
;;   :hook (flycheck-mode . flycheck-popup-tip-mode))

(use-package flyover
  :straight t
  :after flycheck
  ;; :hook (flycheck-mode . flyover-mode) ;; enable this manually
  :custom
  (flyover-virtual-line-type 'line-no-arrow)
  (flyover-show-virtual-line t)
  :custom-face
  (flyover-error   ((t (:weight normal))))
  (flyover-warning ((t (:weight normal))))
  (flyover-info    ((t (:weight normal)))))



(with-eval-after-load 'flycheck
  ;; Use larger or custom icons
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [256 256 256 256 256 256 256 256] 16 16 'center)

  (define-fringe-bitmap 'test-fringe
    ;; Each row is 16 bits (two bytes). 1 means “pixel on,” 0 means “pixel off.”
    [ #b1111111111111111  ;; row1
      #b1111111111111111  ;; row2
      #b1111111111111111  ;; row3
      #b1111111111111111  ;; row4
      #b1111111111111111  ;; row5
      #b1111111111111111  ;; row6
      #b1111111111111111  ;; row7
      #b1111111111111111  ;; row8
      #b1111111111111111  ;; row9
      #b1111111111111111  ;; row10
      #b1111111111111111  ;; row11
      #b1111111111111111  ;; row12
      #b1111111111111111  ;; row13
      #b1111111111111111  ;; row14
      #b1111111111111111  ;; row15
      #b1111111111111111  ;; row16
      ]
    16 16  ;; 8 rows tall, 8 bits wide
    '(center repeated))

  ;; Customize error, warning, info icons
  (flycheck-define-error-level 'error
    :severity 100
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'test-fringe
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 50
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'test-fringe
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 10
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'test-fringe
    :fringe-face 'flycheck-fringe-info))

