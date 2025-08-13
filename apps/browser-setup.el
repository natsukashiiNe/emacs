(use-package eww-lnum
  :after eww
  :bind (:map eww-mode-map ("f" . eww-lnum-follow)))

(use-package shrface
  :after eww
  :hook (eww-after-render . shrface-mode))

(use-package ace-link
  :after eww
  :bind (:map eww-mode-map ("o" . ace-link-eww)))

(use-package xwwp-full
  :after xwidget
  :defer t)      ;; only loads if you compiled with xwidgets
;;(optional)  (use-package eaf :defer t)

;; Sensible EWW defaults
(setq eww-download-directory "~/Downloads"
      shr-max-image-proportion 0.8
      eww-header-line-format "%t  —  %T")

(setq browse-url-browser-function 'xwidget-webkit-browse-url)
