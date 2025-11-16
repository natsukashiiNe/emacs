;;; reading-tools.el --- Book reading configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for reading ebooks (EPUB, FB2) in Emacs
;; Uses nov.el for EPUB files and fb2-reader for FB2 files

;;; Code:

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
:hook (nov-mode . (lambda ()
                    (visual-line-mode 1)
                    (display-line-numbers-mode -1)
                    (hide-mode-line-mode )))
  :config
  (setq nov-text-width     t)
  (setq nov-variable-pitch t)
  
  (defun my/nov-increase-width ()
    "Increase text width in nov-mode."
    (interactive)
    (setq nov-text-width (+ nov-text-width 10))
    (nov-render-document))
  
  (defun my/nov-decrease-width ()
    "Decrease text width in nov-mode."
    (interactive)
    (setq nov-text-width (max 40 (- nov-text-width 10)))
    (nov-render-document)))

(use-package fb2-reader
  :ensure t
  :mode ("\\.fb2\\'" . fb2-reader-mode)
  :config
  (defun my/fb2-next-section ()
    "Go to next section in fb2-reader-mode."
    (interactive)
    (if (fboundp 'fb2-reader-next-section)
        (fb2-reader-next-section)
      (forward-paragraph)))
  
  (defun my/fb2-prev-section ()
    "Go to previous section in fb2-reader-mode."
    (interactive)
    (if (fboundp 'fb2-reader-previous-section)
        (fb2-reader-previous-section)
      (backward-paragraph)))
  
  (defun my/fb2-goto-toc ()
    "Show table of contents in fb2-reader-mode."
    (interactive)
    (if (fboundp 'fb2-reader-goto-toc)
        (fb2-reader-goto-toc)
      (beginning-of-buffer))))

;;; Keybindings

(with-eval-after-load 'nov
  (evil-define-key 'normal nov-mode-map
    (kbd "n") 'nov-next-document
    (kbd "p") 'nov-previous-document
    (kbd "g t") 'nov-goto-toc
    (kbd "g g") 'beginning-of-buffer
    (kbd "G") 'end-of-buffer
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "RET") 'nov-browse-url
    (kbd "C-j") 'evil-scroll-line-down
    (kbd "C-k") 'evil-scroll-line-up
    (kbd "+") 'my/nov-increase-width
    (kbd "-") 'my/nov-decrease-width
    (kbd "r") 'nov-render-document))

(with-eval-after-load 'fb2-reader
  (evil-define-key 'normal fb2-reader-mode-map
    (kbd "n") 'my/fb2-next-section
    (kbd "p") 'my/fb2-prev-section
    (kbd "g t") 'my/fb2-goto-toc
    (kbd "g g") 'beginning-of-buffer
    (kbd "G") 'end-of-buffer
    (kbd "SPC") 'scroll-up-command
    (kbd "S-SPC") 'scroll-down-command
    (kbd "C-j") 'evil-scroll-line-down
    (kbd "C-k") 'evil-scroll-line-up
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line))

;; pdf
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  
  ;; Better rendering quality
  (setq-default pdf-view-display-size 'fit-width)
  (setq pdf-view-resize-factor 1.1)
  
  ;; More responsive
  (setq pdf-view-use-scaling t
        pdf-view-use-imagemagick nil)
  ;; Disable incompatible modes
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)  ; Disable line numbers
              (blink-cursor-mode -1)        ; Optional: disable blinking cursor
              (hide-mode-line-mode )))
  
  ;; Midnight mode (dark mode)
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
  
  ;; Keybindings
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll))

(provide 'reading-tools)
;;; reading-tools.el ends here
