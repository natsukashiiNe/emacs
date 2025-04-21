;; font
(use-package all-the-icons
  :ensure t)

(set-face-attribute 'default nil :font "GoMono Nerd Font-24")
(set-face-attribute 'variable-pitch nil :font "GoMono Nerd Font-24")

;; transparency
(add-to-list 'default-frame-alist '(alpha-background . 92)) ;; 0 = fully transparent, 100 = opaque

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha-set)))
    (background-frame-parameter nil 'alpha-background (if (= alpha 100) 92 100))))

(set-frame-parameter nil 'alpha-background 92)


;; mode-line
