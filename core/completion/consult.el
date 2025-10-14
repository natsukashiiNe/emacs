(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line))
  :config
  (setq consult-project-root-function #'vc-root-dir
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-projectile
  :straight t
  :after (consult projectile)
  :config
  ;; Enable automatic preview
  (setq consult-preview-key 'any) ; Preview on any key
  ;; Configure preview behavior
  (consult-customize
   consult-projectile-find-file
   :preview-key '(:debounce 0.2 any)
   :initial (lambda () (thing-at-point 'filename))))

;; Add preview scrolling keys
;; (define-key consult-preview-mode-map (kbd "C-u") #'consult-preview-scroll-up)
;; (define-key consult-preview-mode-map (kbd "C-d") #'consult-preview-scroll-down)

(use-package consult-lsp)
(use-package consult-flycheck)
(use-package consult-flyspell)


