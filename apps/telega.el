(use-package telega
  :defer t  ; Add this
  :config
  (setq telega-emoji-use-images t)
  (setq telega-emoji-font-family "Noto Color Emoji")
  ;; Suppress telega compilation warnings
  (with-eval-after-load 'comp
    (add-to-list 'native-comp-deferred-compilation-deny-list "telega-.*\\.el$"))
  :hook
  ((telega-root-mode . (lambda () (display-line-numbers-mode -1)))
   (telega-chat-mode . (lambda () (display-line-numbers-mode -1)))))
