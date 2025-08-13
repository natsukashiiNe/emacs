;; ----------------------------
;; UI Customizations
;; ----------------------------
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/") 
(menu-bar-mode 0)    ;; Disable the menu bar
(tool-bar-mode 0)    ;; Disable the toolbar
(scroll-bar-mode 0)  ;; Disable visible scrollbar
(tooltip-mode 0)     ;; Disable tooltips
(set-fringe-mode 10) ;; Give some breathing room
(global-display-line-numbers-mode -1) ;; remove column number (use avy instead)

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
;; Line Numbers & Cursor Settings
;; ----------------------------
(global-hl-line-mode 1)                      ;; Highlight the current line globally
;; (setq display-line-numbers-type t)           ;; Use actual lines, not visual wrapped lines
;; (global-display-line-numbers-mode 1)
(show-paren-mode 1)                          ;; Highlight matching



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
(setq scroll-margin 5
      scroll-conservatively 101
      scroll-step 1)

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

