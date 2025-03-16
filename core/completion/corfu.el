(use-package corfu
  :straight t
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
