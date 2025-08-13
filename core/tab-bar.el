(use-package modern-tab-bar
  :straight (modern-tab-bar :type git :host github :repo "aaronjensen/emacs-modern-tab-bar")
  :init
  (setq tab-bar-show t
        tab-bar-new-button nil
        tab-bar-close-button-show nil)
  (modern-tab-bar-mode))


(set-face-attribute 'modern-tab-bar nil :box nil)
(set-face-attribute 'modern-tab-bar-separator nil :height 1)
