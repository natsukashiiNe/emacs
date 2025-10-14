;; TODO make it not suck

(use-package corfu
  :straight t
  :config
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if appropriate."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer)
  :custom
  (corfu-auto t)                 ;; Enable auto-completion
  (corfu-cycle t)                ;; Enable cycling through candidates
  (corfu-auto-prefix 1)          ;; Minimum prefix length before auto-complete triggers
  (corfu-auto-delay 0.0)         ;; No delay before suggestions appear
  (corfu-preview-current nil)    ;; Disable inline preview
  (corfu-quit-no-match 'separator) ;; Quit when input does not match
  :init
  (global-corfu-mode)) 

(use-package corfu-popupinfo
  :straight (:type git :host github :repo "minad/corfu")
  :after corfu
  :init
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :after corfu
  :config
  ;; Add file completion to completion-at-point-functions
  (add-to-list 'completion-at-point-functions #'cape-file)
  
  ;; Trigger file completion after certain patterns
  (defun my/smart-cape-file ()
    "Provide file completion after file: links or paths."
    (when (or (looking-back "\\[\\[file:" 7)
              (looking-back "~/" 2)
              (looking-back "\\./" 2)
              (looking-back "/" 1))
      (cape-file)))
  
  (add-to-list 'completion-at-point-functions #'my/smart-cape-file))

(use-package corfu-prescient
  :straight t
  :after corfu
  :config
  (corfu-prescient-mode 1))


