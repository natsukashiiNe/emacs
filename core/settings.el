(require 'mode-local)
;; ----------------------------
;; UI Customizations
;; ----------------------------
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/") 
(menu-bar-mode 0)    ;; Disable the menu bar
(tool-bar-mode 0)    ;; Disable the toolbar
(scroll-bar-mode 0)  ;; Disable visible scrollbar
(tooltip-mode 0)     ;; Disable tooltips
(set-fringe-mode 10) ;; Give some breathing room

(setq inhibit-startup-message t) ;; Do not show startup screen
(setq visible-bell nil) ;; No visual bell

;; Load theme
(use-package doom-themes)

(blink-cursor-mode 0)
;; enalble theme
;; (load-theme 'test t)
;; (set-face-background 'child-frame-border "#FF8020")

;; (load-config-file "themes/modeline-options.el") ;; TODO

;; colorizer
(use-package colorful-mode
  :init
  (setq colorful-use-prefix nil) ;; If non-nil, use prefix for preview color instead highlight them.)
)

;; ----------------------------
;; Line Numbers & Folding
;; ----------------------------
(global-hl-line-mode 1)                      ;; Highlight the current line globally
;; (setq display-line-numbers-type t)           ;; Use actual lines, not visual wrapped lines
;; (global-display-line-numbers-mode 1)
(show-paren-mode 1)                          ;; Highlight matching
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'visual)
(add-hook 'visual-line-mode-hook
  (lambda () (setq wrap-prefix
    (propertize "   " 'face 'font-lock-comment-face))))



(use-package hide-mode-line
  :ensure t
  ;;:hook (vdiff-mode . hide-mode-line-mode)
  )


;; ----------------------------
;; Testing sector
;; ----------------------------
;; (global-display-line-numbers-mode t)
;; (setq display-line-numbers-type 'relative)
;; (setq display-line-numbers-current-absolute nil)

;; ----------------------------

;; Ensure that wrapped lines are counted as a single line in the display
(setq display-line-numbers-type 'visual)

;; parentheses
(global-visual-line-mode 1) ;; Enable word wrapping
(column-number-mode 1) ;; Show column number in modeline
(setq visible-cursor nil) ;; Hide the cursor when inactive
(blink-cursor-mode 0)  ;; Completely disable cursor blinking

;; ----------------------------
;; Scrolling & Navigation
;; ----------------------------
(setq scroll-margin 2
      scroll-conservatively 101
      scroll-step 1)

;; TODO: not works now
(require 'mode-local)

;; Make vars survive compilation buffer refreshes
(dolist (sym '(scroll-margin scroll-conservatively scroll-preserve-screen-position))
  (put sym 'permanent-local t))

;; Per-mode: kill margins/recentering in popup-ish modes
(setq-mode-local compilation-mode
  scroll-margin 0
  scroll-conservatively 101
  scroll-preserve-screen-position 'always
  next-error-recenter nil)        ;; avoid recenters when jumping with next-error

(setq-mode-local vterm-mode
  scroll-margin 0
  scroll-conservatively 101
  scroll-preserve-screen-position 'always)

;; Per-mode: disable line numbers (works even with global-display-line-numbers-mode on)
(setq-mode-local vterm-mode       display-line-numbers nil)
(setq-mode-local compilation-mode display-line-numbers nil)

;; ----------------------------
;; Editing & Interaction
;; ----------------------------
(fset 'yes-or-no-p 'y-or-n-p) ;; Make `y` and `n` confirm instead of `yes` and `no`
(setq undo-limit 80000000
      undo-strong-limit 120000000) ;; Better undo system
;; Adaptive Wrap Mode for better text wrapping
(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;; ----------------------------
;; Editing & Interaction
;; ----------------------------
(use-package command-log-mode)

;; ----------------------------
;; Help enhancments
;; ----------------------------
(use-package helpful
  :ensure t
  :bind (;; replace built-ins
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key]      . helpful-key)
         ([remap describe-symbol]   . helpful-symbol)
         ;; extras
         ))

;; 
;; Core popper
(use-package popper
  :ensure t
  :bind (("M-I"   . popper-toggle)        ;; toggle latest popup
         ("M-`"   . popper-cycle)         ;; cycle popups
         ("C-M-`" . popper-toggle-type))  ;; promote/demote popup
  :init
  ;; Which buffers are popups (modes, regexes, or predicates).
  (setq popper-reference-buffers
        '(helpful-mode                      ; *Help*, *helpful: …*
          "\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*Compile-Log\\*"
          compilation-mode
          "\\*grep\\*" "\\*ripgrep\\*" occur-mode xref--xref-buffer-mode
          "\\*Flycheck errors\\*"
          (lambda (buf) (with-current-buffer buf
                          (derived-mode-p 'flymake-diagnostics-buffer-mode)))
          (lambda (buf) (with-current-buffer buf
                          (or (derived-mode-p 'eshell-mode)
                              (derived-mode-p 'shell-mode)
                              (derived-mode-p 'comint-mode))))))
  ;; Let popper control display
  (setq popper-display-control t)
  (setq popper-display-function #'popper-display-popup-at-bottom)
  (setq popper-window-height 0.25)        ; number or (lambda (buf) ...)
  ;; Keep cycling scoped to the current project (uses project.el)
  (setq popper-group-function #'popper-group-by-project)
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(defun my/popper-no-scroll-margin ()
  (setq-local scroll-margin 0))

(add-hook 'popper-mode-hook
          (lambda ()
            ;; whenever popper displays a buffer, fix its margin
            (add-hook 'popper-after-pop-hook #'my/popper-no-scroll-margin)))


;; COLORS
;; Remove any previous colorizers to avoid conflicts
(remove-hook 'compilation-filter-hook #'my/xterm-color-compilation-filter)
;; or, if you added it differently:
(remove-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(require 'ansi-color)
(defun my/ansi-colorize-compilation ()
  (when (derived-mode-p 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))
(add-hook 'compilation-filter-hook #'my/ansi-colorize-compilation)

;; (use-package shackle
;;   :ensure t
;;   :custom
;;   ;; compilation is a bottom popup we don't select; make it dedicated
;;   (shackle-rules
;;    '((compilation-mode :align below :size 0.25 :select nil :popup t :dedicated t)
;;      ;; default rule: when a *file* buffer is displayed (e.g., from errors),
;;      ;; use the other window (your main one), not the popup
;;      ("\\`[^*].*\\'" :regexp t :other t)))
;;   :config
;;   (shackle-mode 1))
