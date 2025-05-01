;;; exu-theme.el 

(deftheme test
  "Exusia is the best girl")

;; exu def colors
(defconst exu-pink             "#FF67AB") ;; functions
(defconst exu-pink-accent1     "#D12F55") ;; alternate pink (e.g. for types)
(defconst exu-pink-accent2     "#F00167") ;; an accent pink
(defconst exu-yellow           "#FFDF40") ;; used for keywords/statements

;; rare exu
(defconst exu-dim-pink         "#C83872") ;; used for cursor lines/bars
(defconst exu-orange-light     "#D19A66") ;; used for some backgrounds
(defconst exu-orange           "#ED997D") ;; used for strings, backgrounds
(defconst exu-orange-accent1   "#FF8700") ;; bright orange accent
(defconst exu-orange-accent2   "#FF8020") ;; used e.g. for MatchParen

;; common colors
(defconst exu-red              "#e86671")
(defconst exu-bred             "#CC0C0C")
(defconst exu-bgreen           "#10E010")
(defconst exu-bgreen2          "#60DC60")
(defconst exu-bcyan            "#1FCFF1")
(defconst exu-cyan             "#56b6c2")
(defconst exu-green            "#71AA71")
(defconst exu-black            "#000000")
(defconst exu-white            "#FFFFFF")
(defconst exu-lpurple          "#c895c8") ;; line numbers, indentation
(defconst exu-dim-purple       "#784f78") ;; inactive lines
(defconst exu-dim-viol         "#a884e0") ;; comments
(defconst exu-blue             "#65D1FF")
(defconst exu-lblue            "#5788FF")
(defconst exu-bg-lred          "#442533") ;; sometimes for current line

;; backgrounds
;; (defconst exu-bg-main "#020E16")  ;; main background
(defconst exu-bg-main          "#000000") ;; main background
(defconst exu-bg-dark          "#1E2836") ;; used for mode bars
(defconst exu-bg-dark2         "#0e1621") ;; used for mode bars

(defconst exu-bg-light         "#181830") ;; used for current line
;;(defconst exu-bg-hover         "#072337") ;; popups
(defconst exu-bg-hover         "#17212b") ;; used for mode bars
(defconst exu-bg-sec           "#16202B") ;; occasional backgrounds
(defconst exu-bg-purple        "#451020") ;; pop-ups
(defconst exu-bg-dark-purple   "#321932") ;; ?
(defconst exu-bg-blue          "#244561") ;; for visual mode
(defconst bg_lred              "#442533")
(defconst tg-dark              "#101018")

;; gre
(defconst exu-grey             "#909090")      ;; info
(defconst exu-grey-text        "#64C4FF") ;; diag info
;;(defconst exu-grey1            "#AAAAAA")
(defconst exu-grey1            "#9D9D9D")
(defconst exu-grey2            "#CACACA")

;;
;; 2) DEFINE THE FACES
;;

(custom-theme-set-faces
 'test

 ;; Basic Editor UI
 `(default         ((t (:foreground ,exu-white    :background ,exu-bg-main))))
 ;;`(default         ((t (:foreground ,exu-white    :background nil))))
 `(hl-line         ((t (:background ,exu-bg-light :foreground unspecified :underline (:color ,exu-orange-accent2)))))
 `(cursor          ((t (:background ,exu-orange-accent2))))
 `(fringe          ((t (:background ,exu-bg-main))))
 `(region          ((t (:background ,exu-bg-blue :foreground ,exu-white :extend t))))
 `(highlight       ((t (:background ,exu-bg-light :foreground ,exu-white :extend t))))
 `(match           ((t ( :box (:line-width 2 :color "#FFFF00" :style released-button )))))
 `(link            ((t (:foreground ,exu-orange :box (:line-width 2 :color ,exu-grey :style flat-button)))))
 `(vertical-border ((t (:foreground ,exu-dim-pink :background nil))))
 `(minibuffer-prompt ((t (:foreground ,exu-black :background ,exu-orange-accent2))))

 ;; `(child-frame-border ((t ( :foreground ,exu-dim-pink))))

 ;; line numbers
 `(line-number ((t (:foreground ,exu-lpurple :background ,exu-bg-main))))
 ;;`(line-number ((t (:foreground ,exu-lpurple :background nil))))
 `(line-number-current-line ((t (:foreground ,exu-dim-pink :background ,exu-bg-light))))

 ;; Syntax (font-lock)
 `(font-lock-comment-face       ((t (:foreground ,exu-dim-viol :slant italic))))
 `(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 `(font-lock-doc-face           ((t (:foreground ,exu-grey1))))
 `(font-lock-string-face        ((t (:foreground ,exu-orange :slant italic))))
 `(font-lock-keyword-face       ((t (:foreground ,exu-yellow))))
 `(font-lock-builtin-face       ((t (:foreground ,exu-yellow :slant italic))))
 `(font-lock-type-face          ((t (:foreground ,exu-pink-accent1))))
 `(font-lock-function-name-face ((t (:foreground ,exu-pink))))
 `(font-lock-variable-name-face ((t (:foreground ,exu-white))))
 `(font-lock-constant-face      ((t (:foreground ,exu-cyan :slant italic))))
 `(font-lock-operator-face      ((t (:foreground ,exu-white))))
 `(font-lock-warning-face       ((t (:foreground ,exu-orange-accent2 :weight bold))))
 `(font-lock-preprocessor-face       ((t (:inherit font-lock-keyword-face))))
 

 ;; LSP
 ;;`(tree-sitter-hl-face:attribute                         
 `(tree-sitter-hl-face:comment                   ((t (:inherit font-lock-comment-face))))
 `(tree-sitter-hl-face:constant                  ((t (:inherit font-lock-constant-face))))
 `(tree-sitter-hl-face:constant.builtin          ((t (:inherit font-lock-constant-face))))
 `(tree-sitter-hl-face:constructor               ((t (:inhert font-lock-function-name-face))))
 `(tree-sitter-hl-face:doc                       ((t (:inherit font-lock-doc-face))))
 `(tree-sitter-hl-face:embedded                  ((t (:inherit font-lock-variable-name-face :slant italic))))
 `(tree-sitter-hl-face:escape                    ((t (:foreground ,exu-cyan :slant italic)) ))
 `(tree-sitter-hl-face:function                  ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:function.builtin          ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:function.call             ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:function.macro            ((t (:foreground ,exu-cyan :slant italic))))
 `(tree-sitter-hl-face:function.special          ((t (:inherit font-lock-function-name-face))))
 ;; `(tree-sitter-hl-face:keyword                            ;;
 `(tree-sitter-hl-face:label                     ((t (:inherit font-lock-variable-name-face))))
 `(tree-sitter-hl-face:method                    ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:method.call               ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:number                    ((t (:foreground ,exu-cyan :slant italic))))
 ;;`(tree-sitter-hl-face:operator                  ((t (:inherit font-lock-keyword-face))))
 `(tree-sitter-hl-face:operator                  ((t (:inherit font-lock-variable-name-face))))
 `(tree-sitter-hl-face:property                  ((t (:inherit font-lock-variable-name-face))))
 `(tree-sitter-hl-face:property.definition       ((t (:inherit font-lock-variable-name-face))))
 `(tree-sitter-hl-face:punctuation               ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:punctuation.bracket       ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:punctuation.delimiter     ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:punctuation.special       ((t (:inherit font-lock-function-name-face))))
 `(tree-sitter-hl-face:string                    ((t (:foreground ,exu-orange))))
 `(tree-sitter-hl-face:string.special            ((t (:foreground ,exu-cyan))))
 `(tree-sitter-hl-face:tag                       ((t (:background "#FFFFFF"))))
 `(tree-sitter-hl-face:type                      ((t (:inherit font-lock-type-face))))
 `(tree-sitter-hl-face:type.argument             ((t (:inherit font-lock-type-face))))
 `(tree-sitter-hl-face:type.builtin              ((t (:inherit font-lock-type-face))))                     
 `(tree-sitter-hl-face:type.parameter            ((t (:inherit font-lock-type-face))))
 `(tree-sitter-hl-face:type.supe                 ((t (:inherit font-lock-type-face))))
 `(tree-sitter-hl-face:variable                  ((t (:inherit font-lock-variable-name-face))))
 ;;`(tree-sitter-hl-face:variable                  ((t (:foreground ,exu-bgreen))))
 `(tree-sitter-hl-face:variable.builtin          ((t (:inherit font-lock-variable-name-face))))
 `(tree-sitter-hl-face:variable.parameter        ((t (:inherit font-lock-variable-name-face))))
 `(tree-sitter-hl-face:variable.special          ((t (:inherit font-lock-variable-name-face))))


 `(lsp-face-highlight-textual   ((t (:background unspecified
                                                 :foreground unspecified
                                                 :box (:line-width 2 :color ,exu-orange-accent2 :style released-button)))))

 `(lsp-face-highlight-read   ((t (:background unspecified
                                              :foreground unspecified
                                              :box (:line-width 2 :color ,exu-orange-accent2 :style released-button)))))
 `(lsp-face-highlight-write   ((t (:background unspecified
                                               :foreground unspecified
                                               :box (:line-width 2 :color ,exu-orange-accent2 :style released-button)))))

 ;;`(lsp-face-rename                  ((t () )))
 `(lsp-face-semhl-class             ((t (:inherit font-lock-type-face) )))
 `(lsp-face-semhl-comment           ((t (:inherit font-lock-comment-face) )))
 `(lsp-face-semhl-constant          ((t (:foreground nil :background nil) )))
 `(lsp-face-semhl-default-library   ((t (:inherit font-lock-type-face) )))
 `(lsp-face-semhl-definition        ((t (:foreground nil :background nil))))
 ;;`(lsp-face-semhl-deprecated        ((t () )))
 `(lsp-face-semhl-enum              ((t (:inherit font-lock-constant-face) )))
 ;;`(lsp-face-semhl-event             ((t () )))
 `(lsp-face-semhl-function           ((t (:inherit font-lock-function-name-face) )))
 ;;`(lsp-face-semhl-implementation    ((t () )))
 `(lsp-face-semhl-interface         ((t (:foreground nil :background nil) )))
 `(lsp-face-semhl-keyword           ((t (:inherit font-lock-keyword-face) )))
 ;;`(lsp-face-semhl-label             ((t () )))
 `(lsp-face-semhl-macro             ((t (:inherit tree-sitter-hl-face:function.macro) )))
 `(lsp-face-semhl-member            ((t (:inherit font-lock-variable-name-face) )))
 `(lsp-face-semhl-method            ((t (:inherit font-lock-function-name-face) )))
 `(lsp-face-semhl-namespace         ((t (:inherit font-lock-type-face) )))
 `(lsp-face-semhl-number            ((t (:inherit font-lock-constant-face) )))
 `(lsp-face-semhl-operator          ((t (:inherit font-lock-operator-face) )))
 `(lsp-face-semhl-parameter         ((t (:inherit tree-sitter-hl-face:type.parameter) )))
 `(lsp-face-semhl-parameter         ((t (:inherit font-lock-variable-name-face) )))
 `(lsp-face-semhl-property          ((t (:inherit font-lock-function-name-face) )))
 `(lsp-face-semhl-regexp            ((t () )))
 `(lsp-face-semhl-static            ((t () )))
 `(lsp-face-semhl-string            ((t (:inherit font-lock-string-face) )))
 `(lsp-face-semhl-struct            ((t (:inherit font-lock-type-face) )))
 `(lsp-face-semhl-type              ((t (:inherit font-lock-type-face) )))
 `(lsp-face-semhl-type-parameter    ((t ( :inherit tree-sitter-hl-face:type.parameter) )))

 ;; `(lsp-face-semhl-variable

 ;; etc. (numbers, booleans) if your major-modes split them out:
 ;; (font-lock-number-face   ((t (:foreground ,exu-cyan :slant italic))))
 ;; (font-lock-boolean-face  ((t (:foreground ,exu-cyan :weight bold))))

 ;; Mode line / tab line
 `(mode-line
   ((t (:foreground ,exu-white :background ,exu-bg-dark
                    :box (:line-width 2 :color ,exu-bg-dark)))))
 `(mode-line-inactive
   ((t (:foreground ,exu-dim-viol  :background ,exu-bg-main
                    :box (:line-width 1 :color ,exu-dim-viol)))))

 `(persp-selected-face
   ((t (:background ,exu-bg-light :foreground ,exu-pink-accent2))))

 ;; lines

 `(header-line                         ((t (:background ,exu-bg-dark2 ))))
 
 ;; tabine
 `(tab-bar                         ((t (:foreground ,exu-white :background ,exu-bg-dark ))))
 `(tab-bar-tab                     ((t (:foreground ,exu-pink-accent2 :background ,exu-black) )))
 ;;`(tab-bar-tab-group-current       ((t (:foreground ,exu-pink-accent1 :background ,exu-black) )))
 ;;`(tab-bar-tab-group-inactive      ((t () )))
 `(tab-bar-tab-inactive            ((t (:foreground ,exu-yellow :background ,exu-bg-dark) )))
 `(tab-bar-tab-ungrouped           ((t () )))
 ;;`(tab-line                        ((t (:foreground "#FFFFFF") )))

 ;; Matching parens, Search
 `(show-paren-match
   ((t (:foreground ,exu-orange-accent2 :weight bold))))
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
   ((t (:foreground ,exu-pink-accent1 :weight bold))))
 `(which-key-group-description-face
   ((t (:foreground ,exu-orange-accent2))))
 `(which-key-separator-face
   ((t (:foreground ,exu-dim-pink))))
 `(which-key-command-description-face
   ((t (:foreground ,exu-orange :slant italic))))
 `(which-key-note-face
   ((t (:foreground ,exu-pink :slant italic))))

 ;; Completion (Vertico, Corfu, Orderless, etc.)
 `(vertico-current ((t (:inherit hl-line))))
 `(vertico-group-title ((t (:foreground ,exu-orange-accent2))))

 `(corfu-default
   ((t (:background ,exu-bg-hover :foreground ,exu-white))))
 `(corfu-current
   ((t (:background ,bg_lred :foreground ,exu-white
                    :slant italic :underline t))))
 `(corfu-border
   ((t (:background ,exu-bg-purple))))
 
 `(orderless-match-face-0
   ((t (:foreground ,exu-pink-accent2 :weight bold))))
 `(orderless-match-face-1
   ((t (:foreground ,exu-yellow :weight bold))))
 `(orderless-match-face-2
   ((t (:foreground ,exu-orange-accent2 :weight bold))))
 `(orderless-match-face-3
   ((t (:foreground ,exu-cyan :weight bold))))

 ;; Embark
 `(embark-keybinding
   ((t (:foreground ,exu-pink-accent2 :weight bold))))

 ;; Marginalia
 `(marginalia-documentation
   ((t (:foreground ,exu-dim-viol :slant italic))))
 `(marginalia-type
   ((t (:foreground ,exu-pink-accent1))))
 `(marginalia-key
   ((t (:foreground ,exu-orange :slant italic))))

 ;; Consult
 `(consult-preview-match
   ((t (:background ,exu-dim-pink :foreground ,exu-black :weight bold))))
 `(consult-line-number
   ((t (:foreground ,exu-lpurple))))
 `(consult-key
   ((t (:foreground ,exu-orange))))

 ;; Org-mode or Neorg-like
 `(org-level-1
   ((t (:foreground ,exu-pink-accent2 :weight bold))))
 `(org-level-2
   ((t (:foreground ,exu-yellow :weight bold))))
 `(org-level-3
   ((t (:foreground ,exu-pink-accent1 :weight bold))))
 `(org-level-4
   ((t (:foreground ,exu-orange :weight bold))))
 `(org-document-title
   ((t (:foreground ,exu-pink-accent1 :weight bold :height 1.3))))
 `(org-code
   ((t (:background ,exu-bg-light :foreground ,exu-cyan :slant italic))))

 `(org-block-begin-line
   ((t (:foreground ,exu-grey1 :background ,tg-dark :box (:line-width 2 :color ,exu-lpurple :style released-button ) :extend t ))))
 `(org-block
   ((t (:background ,tg-dark :foreground ,exu-white))))
 `(org-block-end-line
   ((t (:foreground ,exu-grey1 :background ,tg-dark :box (:line-width 2 :color ,exu-lpurple :style released-button ) :extend t ))))

 ;; groups
 `(org-verbatim ((t ( :foreground ,exu-bgreen2 ))))



 ;; Basic diff
 `(diff-added   ((t (:foreground ,exu-bgreen  :background unspecified))))
 `(diff-changed ((t (:foreground ,exu-yellow  :background unspecified))))
 `(diff-removed ((t (:foreground ,exu-red     :background unspecified))))

 ;; GIT


 ;; TELEGA
 `(telega-highlight-text-face ((t (:box (:line-width 2 :color "#00FF00" :style released-button)))))
 
 )

(provide-theme 'test)
;;; exu-theme.el ends here
