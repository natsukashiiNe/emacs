;; FUNTION TO HOOK ON GUI FRAME

;; cond settings
;; (mapc #'disable-theme custom-enabled-themes)
;; (load-theme 'modus-operandi-tinted)
;; (set-face-background 'child-frame-border "#a60000")

;; cond settings
(mapc #'disable-theme custom-enabled-themes)
;; (load-theme 'modus-vivendi-tinted)
;; (load-theme 'doom-molokai)
;; (set-face-background 'child-frame-border "#a60000")

;; (load-theme 'modus-operandi-tinted)
;; (load-theme 'my-modus-mono)
(load-theme 'my-modus-mono-dark)

;; Fonts
(set-face-attribute 'default nil :font "GoMono Nerd Font-21")
(set-face-attribute 'variable-pitch nil :font "GoMono Nerd Font-20")

;; Transparency
(add-to-list 'default-frame-alist '(alpha-background . 100))
(set-frame-parameter nil 'alpha-background 100)

;; Doom modeline
(use-package all-the-icons :ensure t)
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-modal-icon t))
