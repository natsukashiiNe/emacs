;;; exu-theme.el --- A Neovim-inspired Emacs theme  -*- lexical-binding: t; -*-
;;
;; You can name this file “exu-theme.el” so that Emacs recognizes it as a theme
;; automatically, but it’s not strictly mandatory if you plan to load it manually.

(deftheme test
  "Neovim-inspired Emacs theme using your curated color palette.")

;;
;; 1) COLOR CONSTANTS
;;

;; exu def colors
(defconst exu-pink "#FF67AB") ;; used for prominent function highlights (exu_bright_lpink)
(defconst exu-pink-accent1 "#F00167") ;; an accent pink
(defconst exu-pink-accent2 "#D12F55") ;; alternate pink (e.g. for types)
(defconst exu-yellow "#FFDF40") ;; used for keywords/statements

;; rare exu
(defconst exu-dim-pink "#C83872") ;; used for cursor lines/bars
(defconst exu-orange-light "#D19A66") ;; used for some backgrounds
(defconst exu-orange "#ED997D") ;; used for strings, backgrounds
(defconst exu-orange-accent1 "#FF8700") ;; bright orange accent
(defconst exu-orange-accent2 "#FF8020") ;; used e.g. for MatchParen

;; common colors
(defconst exu-red "#e86671")
(defconst exu-bred "#CC0C0C")
(defconst exu-bgreen "#10E010")
(defconst exu-bgreen2 "#60DC60")
(defconst exu-bcyan "#1FCFF1")
(defconst exu-cyan "#56b6c2")
(defconst exu-black "#000000")
(defconst exu-white "#FFFFFF")
(defconst exu-lpurple "#c895c8") ;; line numbers, indentation
(defconst exu-dim-purple "#784f78") ;; inactive lines
(defconst exu-dim-viol "#a884e0") ;; comments
(defconst exu-blue "#65D1FF")
(defconst exu-lblue "#5788FF")
(defconst exu-bg-lred "#442533") ;; sometimes for current line

;; backgrounds
(defconst exu-bg-main "#020E16")  ;; main background
(defconst exu-bg-dark "#1E2836")  ;; used for mode bars
(defconst exu-bg-light "#181830") ;; used for current line
(defconst exu-bg-hover "#072337") ;; popups
(defconst exu-bg-sec "#16202B")   ;; occasional backgrounds
(defconst exu-bg-purple "#451020") ;; pop-ups
(defconst exu-bg-dark-purple "#321932") ;; ?

;; grey
(defconst exu-grey "#909090")      ;; info
(defconst exu-grey-text "#64C4FF") ;; diag info
(defconst exu-grey1 "#AAAAAA")
(defconst exu-grey2 "#CACACA")

;;
;; 2) DEFINE THE FACES
;;

(custom-theme-set-faces
 'test

 ;; Basic Editor UI
 `(default ((t (:foreground ,exu-white :background ,exu-bg-main))))
 `(cursor  ((t (:background ,exu-dim-pink))))
 `(fringe  ((t (:background ,exu-bg-main))))
 `(region  ((t (:background ,exu-bg-light :foreground ,exu-white :extend t))))
 `(highlight ((t (:background ,exu-bg-light :foreground ,exu-white :extend t))))
 `(vertical-border ((t (:foreground ,exu-dim-pink :background nil))))

 ;; line numbers
 `(line-number ((t (:foreground ,exu-lpurple :background ,exu-bg-main))))
 `(line-number-current-line ((t (:foreground ,exu-dim-pink :background ,exu-bg-light))))

 ;; Syntax (font-lock)
 `(font-lock-comment-face       ((t (:foreground ,exu-dim-viol :slant italic))))
 `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 `(font-lock-doc-face           ((t (:inherit font-lock-comment-face))))
 `(font-lock-string-face        ((t (:foreground ,exu-orange :slant italic))))
 `(font-lock-keyword-face       ((t (:foreground ,exu-yellow))))
 `(font-lock-builtin-face       ((t (:foreground ,exu-yellow :slant italic))))
 `(font-lock-type-face          ((t (:foreground ,exu-pink-accent2))))
 `(font-lock-function-name-face ((t (:foreground ,exu-pink))))
 `(font-lock-variable-name-face ((t (:foreground ,exu-white))))
 `(font-lock-operator-face      ((t (:foreground ,exu-white))))
 `(font-lock-warning-face       ((t (:foreground ,exu-orange-accent2 :weight bold))))

  ;; LSP
  ;;`(tree-sitter-hl-face:attribute                         
  `(tree-sitter-hl-face:comment                   ((t (:inherit font-lock-comment-face))))
  `(tree-sitter-hl-face:constant                  ((t (:inherit font-lock-variable-name-face))))
  `(tree-sitter-hl-face:constant.builtin          ((t (:inherit font-lock-variable-name-face))))
  `(tree-sitter-hl-face:constructor               ((t (:inhert font-lock-function-name-face))))
  `(tree-sitter-hl-face:doc                       ((t (:inherit font-lock-doc-face))))
  `(tree-sitter-hl-face:embedded                  ((t (:foreground ,exu-bgreen))))
  `(tree-sitter-hl-face:escape                    ((t (:foreground ,exu-cyan :slant italic)) ))
  `(tree-sitter-hl-face:function                  ((t (:foreground ,exu-pink))))
  `(tree-sitter-hl-face:function.builtin          ((t (:foreground ,exu-pink))))
  `(tree-sitter-hl-face:function.call             ((t (:foreground ,exu-pink))))
  `(tree-sitter-hl-face:function.macro            ((t (:inherit font-lock-function-name-face))))
  `(tree-sitter-hl-face:function.special          ((t (:inherit font-lock-function-name-face))))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; `(tree-sitter-hl-face:keyword                            ;;
  ;; `(tree-sitter-hl-face:label                              ;;
  `(tree-sitter-hl-face:method                    ((t (:inherit font-lock-function-name-face))))
  `(tree-sitter-hl-face:method.call ((t (:foreground ,exu-pink))))
  ;; `(tree-sitter-hl-face:number                             ;;
  ;; `(tree-sitter-hl-face:operator                           ;;
  ;; `(tree-sitter-hl-face:property                           ;;
  ;; `(tree-sitter-hl-face:property.definition                ;;
  ;; `(tree-sitter-hl-face:punctuation                        ;;
  ;; `(tree-sitter-hl-face:punctuation.bracket                ;;
  ;; `(tree-sitter-hl-face:punctuation.delimiter              ;;
  ;; `(tree-sitter-hl-face:punctuation.special                ;;
  ;; `(tree-sitter-hl-face:string                             ;;
  ;; `(tree-sitter-hl-face:string.special                     ;;
  ;; `(tree-sitter-hl-face:tag                                ;;
  ;; `(tree-sitter-hl-face:type                               ;;
  ;; `(tree-sitter-hl-face:type.argument                      ;;
  ;; `(tree-sitter-hl-face:type.builtin                       ;;
  ;; `(tree-sitter-hl-face:type.parameter                     ;;
  ;; `(tree-sitter-hl-face:type.super                         ;;
  ;; `(tree-sitter-hl-face:variable                           ;;
  ;; `(tree-sitter-hl-face:variable.builtin                   ;;
  ;; `(tree-sitter-hl-face:variable.parameter                 ;;
  ;; `(tree-sitter-hl-face:variable.special                   ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; etc. (numbers, booleans) if your major-modes split them out:
 ;; (font-lock-number-face   ((t (:foreground ,exu-cyan :slant italic))))
 ;; (font-lock-boolean-face  ((t (:foreground ,exu-cyan :weight bold))))

 ;; Mode line / tab line
 `(mode-line
   ((t (:foreground ,exu-white :background ,exu-bg-dark
                    :box (:line-width -1 :color ,exu-bg-dark)))))
 `(mode-line-inactive
   ((t (:foreground ,exu-dim-purple :background ,exu-bg-main
                    :box (:line-width -1 :color ,exu-bg-main)))))

 `(tab-line
   ((t (:background ,exu-bg-dark :foreground ,exu-white))))
 `(tab-line-tab
   ((t (:background ,exu-bg-dark :foreground ,exu-pink-accent2
                    :box (:line-width 1 :color ,exu-dim-pink)))))
 `(tab-line-tab-inactive
   ((t (:background ,exu-bg-main :foreground ,exu-dim-viol
                    :box (:line-width 1 :color ,exu-dim-pink)))))
 `(tab-line-highlight
   ((t (:background ,exu-bg-light :foreground ,exu-white :weight bold))))

 ;; Matching parens, Search
 `(show-paren-match
   ((t (:background ,exu-orange-accent2 :foreground ,exu-black :weight bold))))
 `(show-paren-mismatch
   ((t (:background ,exu-red :foreground ,exu-white :weight bold))))

 `(isearch
   ((t (:background ,exu-dim-pink :foreground ,exu-black :weight bold))))
 `(lazy-highlight
   ((t (:background ,exu-bg-light :foreground ,exu-white :underline t))))

 ;; LSP / Diagnostics (flymake, etc.)
 `(flymake-error
   ((t (:underline (:style wave :color ,exu-bcyan)))))
 `(flymake-warning
   ((t (:underline (:style wave :color ,exu-orange-accent2)))))
 `(flymake-note
   ((t (:underline (:style wave :color ,exu-grey-text)))))

 ;; If you use Flyspell:
 `(flyspell-incorrect
   ((t (:underline (:style wave :color ,exu-red)))))
 `(flyspell-duplicate
   ((t (:underline (:style wave :color ,exu-orange)))))

 ;; If you use lsp-mode:
 `(lsp-face-error
   ((t (:foreground ,exu-bcyan :weight bold))))
 `(lsp-face-warning
   ((t (:foreground ,exu-orange-accent2 :slant italic))))
 `(lsp-face-info
   ((t (:foreground ,exu-grey-text :slant italic))))
 `(lsp-face-hint
   ((t (:foreground ,exu-grey :slant italic))))

 ;; Which-key
 `(which-key-key-face
   ((t (:foreground ,exu-pink-accent2 :weight bold))))
 `(which-key-group-description-face
   ((t (:foreground ,exu-orange-accent2))))
 `(which-key-separator-face
   ((t (:foreground ,exu-dim-pink))))
 `(which-key-command-description-face
   ((t (:foreground ,exu-orange :slant italic))))
 `(which-key-note-face
   ((t (:foreground ,exu-pink :slant italic))))

 ;; Completion (Vertico, Corfu, Orderless, etc.)
 `(vertico-current
   ((t (:background ,exu-bg-dark-purple :foreground ,exu-white
                    :underline t :weight bold))))
 `(corfu-default
   ((t (:background ,exu-bg-hover :foreground ,exu-white))))
 `(corfu-current
   ((t (:background ,exu-bg-dark-purple :foreground ,exu-white
                    :weight bold :underline t))))
 `(corfu-border
   ((t (:background ,exu-bg-purple))))
 
 `(orderless-match-face-0
   ((t (:foreground ,exu-pink-accent1 :weight bold))))
 `(orderless-match-face-1
   ((t (:foreground ,exu-orange :weight bold))))
 `(orderless-match-face-2
   ((t (:foreground ,exu-yellow :weight bold))))
 `(orderless-match-face-3
   ((t (:foreground ,exu-cyan :weight bold))))

 ;; Embark
 `(embark-keybinding
   ((t (:foreground ,exu-pink-accent1 :weight bold))))

 ;; Marginalia
 `(marginalia-documentation
   ((t (:foreground ,exu-dim-viol :slant italic))))
 `(marginalia-type
   ((t (:foreground ,exu-pink-accent2))))
 `(marginalia-key
   ((t (:foreground ,exu-yellow :weight bold))))

 ;; Consult
 `(consult-preview-match
   ((t (:background ,exu-dim-pink :foreground ,exu-black :weight bold))))
 `(consult-line-number
   ((t (:foreground ,exu-lpurple))))

 ;; Org-mode or Neorg-like
 `(org-level-1
   ((t (:foreground ,exu-pink-accent1 :weight bold))))
 `(org-level-2
   ((t (:foreground ,exu-yellow :weight bold))))
 `(org-level-3
   ((t (:foreground ,exu-pink-accent2 :weight bold))))
 `(org-level-4
   ((t (:foreground ,exu-orange :weight bold))))
 `(org-document-title
   ((t (:foreground ,exu-pink-accent2 :weight bold :height 1.3))))
 `(org-block
   ((t (:background ,exu-bg-light :foreground ,exu-white))))
 `(org-code
   ((t (:background ,exu-bg-light :foreground ,exu-cyan :slant italic))))

 ;; Basic diff
 `(diff-added   ((t (:foreground ,exu-bgreen  :background nil))))
 `(diff-changed ((t (:foreground ,exu-yellow  :background nil))))
 `(diff-removed ((t (:foreground ,exu-red     :background nil))))
)

(provide-theme 'test)
;;; exu-theme.el ends here
