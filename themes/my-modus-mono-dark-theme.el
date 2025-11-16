;;; my-modus-mono-dark-theme.el --- Monochrome theme -*- lexical-binding: t; -*-

;;; Commentary:
;; (Almost) monochrome dark theme based on the modus-vivendi-tinted

;;; Code:
(deftheme my-modus-mono-dark
  "A minimal and monochrome-inspired override for modus-operandi-tinted.")

;; Define color constants (mimicking your nvim color approach):
(defconst exu-black       "#000000")

(defconst exu-fg          "#DCDCDC")
(defconst exu-bg          "#0d0e1c")
(defconst exu-accent      "#FF8020")
(defconst exu-accent-weak "#FD994D")
(defconst exu-accent-alt  "#ff7f9f")

;;(defconst exu-main-sup1   "#c7a349")
(defconst exu-main-sup1   "#F6C177")
(defconst exu-main-sup2   "#9ACC8D")
(defconst exu-grey        "#AAAAAA")
(defconst exu-grey2       "#989898")
(defconst exu-comment     "#5e5e5e")
(defconst exu-viol        "#a884e0")

(defconst exu-italic      'italic)
(defconst exu-bold        'bold)

;; DIFFERENT BGS
(defconst exu-bg3 "#1d2235")
(defconst exu-bg-sel "#1a1c35")


;; DIFFRENT FGS
(defconst exu-fg2 "#333333")

;; DIAGNOSTICS COLOR
;; -- Error
(defconst exu-cyan-accent     "#50E5FF")
(defconst exu-cyan-dimmer     "#136F80")
(defconst exu-bg-cyan         "#205158")

;; -- Warn
(defconst exu-magenta-accent  "#f78fe7")
(defconst exu-magenta-dimmer  "#b0589e")
(defconst exu-bg-magenta      "#4a1c4a")

;; -- Error
(defconst exu-green-accent     "#60DC60")
(defconst exu-green-dimmer     "#0B5E30")
(defconst exu-bg-green         "#C7EAD9")
;; -- Warn
(defconst exu-orange-accent    "#FF8700")
(defconst exu-orange-light     "#A64700")
(defconst exu-bg-orange        "#FFD0A0")
;; -- Info
(defconst exu-blue-light       "#1565C0") ;; "#1E88E5","#0D47A1"
(defconst exu-bg-blue          "#D7ECFF")



;; First, load the base theme:
(load-theme 'modus-vivendi-tinted t)

;; Now override syntax and LSP faces:
(custom-theme-set-faces 'my-modus-mono-dark
 `(default ((t  (:background ,exu-bg))))
  ;; DEFAULT AND UI
 `(hl-line ((t  (:background "#1D1D3B" :extend t))))
 `(tab-bar ((t  (:foreground ,exu-accent :background ,exu-bg3 :box nil ))))
 `(tab-bar-tab ((t  (:foreground ,exu-accent :background ,exu-bg3 :box nil ))))
 `(tab-bar-tab-inactive ((t  (:foreground ,exu-grey2 :background ,exu-bg3 :box nil ))))

 `(font-lock-function-name-face ((t  (:foreground ,exu-accent-alt :slant normal :weight normal))))
 `(font-lock-builtin-face       ((t  (:foreground ,exu-accent-alt :slant normal :weight normal))))
 `(font-lock-type-face          ((t  (:foreground ,exu-accent-weak :slant normal :weight normal))))
 `(font-lock-variable-name-face ((t  (:foreground ,exu-fg :slant normal :weight normal))))
 `(font-lock-keyword-face       ((t  (:foreground ,exu-accent-weak :slant normal :weight normal))))
 `(font-lock-string-face        ((t  (:foreground ,exu-main-sup1 :slant normal :weight normal))))
 `(font-lock-comment-face       ((t  (:foreground ,exu-comment :slant ,exu-italic :weight normal))))
 `(font-lock-doc-face           ((t  (:foreground ,exu-viol :slant ,exu-italic :weight normal))))
 `(font-lock-constant-face      ((t  (:foreground ,exu-main-sup2 :slant normal :weight normal))))
 `(font-lock-number-face        ((t  (:foreground ,exu-main-sup2 :slant ,exu-italic :weight normal))))


 ;; LSP faces
 ;; Errors, warnings, etc. can be minimal or softly highlighted
 `(lsp-face-highlight-textual ((t
        (:background unspecified
         :foreground unspecified
         :box (:line-width 1 :color ,exu-accent :style released-button)))))

 `(lsp-face-highlight-read ((t
        (:background unspecified
         :foreground unspecified
         :box (:line-width 1 :color ,exu-accent :style released-button)))))

 `(lsp-face-highlight-write ((t
        (:background unspecified
         :foreground unspecified
         :weight bold
         :box (:line-width 1 :color ,exu-accent :style released-button)))))

 `(lsp-face-error       ((t  (:foreground ,exu-accent :background unspecified :weight ,exu-bold))))
 `(lsp-face-warning     ((t  (:foreground ,exu-accent :background unspecified :slant ,exu-italic))))
 `(lsp-face-info        ((t  (:foreground ,exu-grey :slant ,exu-italic))))
 `(lsp-face-hint        ((t  (:foreground ,exu-grey :slant ,exu-italic))))


 `(lsp-face-hint        ((t  (:foreground ,exu-grey :slant ,exu-italic))))

 `(lsp-face-highlight-read ((t (:background unspecified ))))
 `(lsp-face-highlight-textual ((t (:background unspecified ))))
 `(lsp-face-highlight-write ((t (:background unspecified ))))   ;; LSP semantic tokens

 `(lsp-face-semhl-function   ((t  (:inherit font-lock-function-name-face))))
 `(lsp-face-semhl-member   ((t  (:inherit font-lock-function-name-face))))
 `(lsp-face-semhl-variable   ((t  (:inherit font-lock-variable-name-face))))
 `(lsp-face-semhl-type       ((t  (:inherit font-lock-type-face))))
 `(lsp-face-semhl-parameter  ((t  (:foreground ,exu-fg :slant ,exu-italic))))
 `(lsp-face-semhl-operator   ((t  (:inherit font-lock-keyword-face))))
 `(lsp-face-semhl-string     ((t  (:inherit font-lock-string-face))))
 `(lsp-face-semhl-comment    ((t  (:inherit font-lock-comment-face))))
 `(lsp-face-semhl-namespace  ((t  (:inherit font-lock-type-face))))
 `(lsp-face-semhl-constant   ((t  (:inherit font-lock-constant-face))))
 ;; etc. as needed
 `(tree-sitter-hl-face:method.call ((t (:inherit font-lock-function-name-face))))

 ;; --- FLYCHECK ----------------------------------------------------------------
 ;; inline
 `(flycheck-error            ((t (:background ,exu-bg-cyan :underline nil ))))
 `(flycheck-warning          ((t (:background ,exu-bg-magenta :underline nil :weight ,exu-bold))))
 `(flycheck-info             ((t (:background ,exu-bg-sel :underline nil ))))

 `(flycheck-inline-error     ((t (:foreground ,exu-bg-cyan :background ,exu-cyan-accent :extend t))))
 `(flycheck-inline-warning   ((t (:foreground ,exu-magenta-accent :background ,exu-bg-magenta :weight ,exu-bold :extend t))))
 `(flycheck-inline-info      ((t (:foreground ,exu-blue-light   :background ,exu-bg-blue :extend t ))))

 ;; icons
 `(flycheck-fringe-error     ((t (:foreground ,exu-cyan-accent :background ,exu-bg-orange))))
 `(flycheck-fringe-warning   ((t (:foreground ,exu-magenta-accent  :background ,exu-bg-orange))))
 `(flycheck-fringe-info      ((t (:foreground ,exu-blue-light    :background ,exu-bg-orange))))

 `(flyover-error             ((t (:foreground ,exu-cyan-accent :background ,exu-bg-orange))))
 `(flyover-warning           ((t (:foreground ,exu-magenta-accent  :background ,exu-bg-orange))))
 `(flyover-info              ((t (:foreground ,exu-blue-light    :background ,exu-bg-orange))))

 `(flycheck-overlay-marker             ((t (:foreground ,exu-cyan-accent :background ,exu-bg-orange))))


 ;; --- TELEGA ------------------------------------------------------------------

`(telega-msg-heading ((t (:background ,exu-bg :weight bold))))
`(telega-msg-inline-forward ((t (:background ,exu-bg :slant italic))))

 )
;; --- CUSTOM FACES ------------------------------------------------------------
(defface my-hl-line-normal
  '((t (:background "#F1D5D0" :extend t)))
  "HL line face for Evil normal mode.")

(defface my-hl-line-insert
  '((t (:background "#C7EAD9" :extend t)))
  "HL line face for Evil insert mode.")

(defun evil-hl-line--use (face)
  "Switch `hl-line-face' to FACE and refresh the overlay immediately."
  (setq hl-line-face face)
  ;; Force the existing overlay (if any) to pick up the new face even
  ;; before the point moves.
  (hl-line-highlight))



;; --- HOOKS -------------------------------------------------------------------
;; (defun my-modus-mono--enter-insert-state ()
;;   "Change cursor/line face for Evil insert state."
;;   (when (eq (car custom-enabled-themes) 'my-modus-mono)
;;     (custom-theme-set-faces
;;      'my-modus-mono
;;      '(hl-line ((t (:background ,exu-bg-green :extend t)))))
;;     (setq hl-line-face 'my-hl-line-insert)
;;     (set-face-background 'hl-line exu-bg-green)))

;; (defun my-modus-mono--enter-normal-state ()
;;   "Change cursor/line face for Evil normal state."
;;   (when (eq (car custom-enabled-themes) 'my-modus-mono)
;;     (custom-theme-set-faces
;;      'my-modus-mono
;;      '(hl-line ((t (:background ,exu-bg-pink :extend t)))))
;;     (setq hl-line-face 'my-hl-line-normal)
;;     (set-face-background 'hl-line exu-bg-pink)))


;; ;; --- APPLY HOOKS --------------------------------------------------------------
;; (defun my-modus-mono--apply-evil-hooks ()
;;   "Attach Evil hooks to tweak cursor/line highlighting when my-modus-mono is active."
;;   (add-hook 'evil-normal-state-entry-hook #'my-modus-mono--enter-normal-state)
;;   (add-hook 'evil-insert-state-entry-hook #'my-modus-mono--enter-insert-state))

;; (defun my-modus-mono--remove-evil-hooks ()
;;   "Detach Evil hooks, restoring original states."
;;   (remove-hook 'evil-normal-state-entry-hook #'my-modus-mono--enter-normal-state)
;;   (remove-hook 'evil-insert-state-entry-hook #'my-modus-mono--enter-insert-state))

;; ;; --- ENABLE/DISABLE HOOKS FOR THE THEME ---------------------------------------
;; (defun my-modus-mono--post-enable-theme (theme &rest _)
;;   (when (eq theme 'my-modus-mono)
;;     (global-hl-line-mode 1)
;;     (setq hl-line-face 'my-hl-line-normal)
;;     (my-modus-mono--apply-evil-hooks)))
;; (advice-add 'enable-theme :after #'my-modus-mono--post-enable-theme)

;; (defun my-modus-mono--post-disable-theme (theme &rest _)
;;   (when (eq theme 'my-modus-mono)
;;     (my-modus-mono--remove-evil-hooks)))
;; (advice-add 'disable-theme :after #'my-modus-mono--post-disable-theme)

(provide-theme 'my-modus-mono-dark)
;;; my-modus-mono-dark-theme.el ends here
