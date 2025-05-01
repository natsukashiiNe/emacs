(setq major-mode-remap-alist
      '((c-mode  . c-ts-mode)
        (c++-mode . c++-ts-mode)))

(use-package lsp-mode
  :hook ((c-ts-mode  . my/c‑setup-lsp)
         (c++-ts-mode . my/c‑setup-lsp))
  :custom
  ;; do NOT fight with flymake/flycheck here
  (lsp-semantic-tokens-enable t)          ; always request tokens
  (lsp-semantic-tokens-apply-modifiers t) ; we’ll tame them next
  :init
  (defun my/c‑setup-lsp ()
    (lsp-deferred)                        ; starts clangd
    (lsp-semantic-tokens-mode 1)))

;; 3a  — pure kinds -----------------------------------------------------------
;; (setq lsp-semantic-token-faces
;;       '((class        . font-lock-type-face)
;;         (struct       . font-lock-type-face)
;;         (enum         . font-lock-type-face)
;;         (enumMember   . font-lock-constant-face)   ; cyan
;;         (function     . font-lock-function-name-face)
;;         (method       . font-lock-function-name-face) ; pink
;;         (parameter    . font-lock-variable-name-face)
;;         (property     . font-lock-variable-name-face)
;;         (variable     . font-lock-variable-name-face)
;;         (macro        . font-lock-preprocessor-face)
;;         (namespace    . font-lock-type-face)
;;         (type         . font-lock-type-face)))

;; 3b  — *modifiers* are what turned `get()` white ---------------------------
;; Clangd slaps “readonly”, “static” and “const” modifiers on lots of tokens.
;; When `lsp-semantic-tokens-apply-modifiers` is non‑nil those extra faces
;; *merge* with the base face and often win the colour war.
;; (setq lsp-semantic-token-modifier-faces
;;       '((readonly . font-lock-constant-face) ; const variables → cyan
;;         (static   . font-lock-constant-face) ; static vars     → cyan
;;         ;; all other modifiers inherit NOTHING -> they stop overriding colours
;;         (deprecated . nil) (abstract . nil) (defaultLibrary . nil)))
;; 
;; (require 'treesit)
;; 
;; (dolist (lang '(c cpp))
;;   (treesit-font-lock-recompute-features
;;    lang
;;    `((preproc_def               name: (identifier) @font-lock-preprocessor-face)
;;      (preproc_function_def
;;       name: (identifier)     @font-lock-preprocessor-face
;;       parameters: (preproc_params (identifier) @font-lock-variable-name-face)))))
;; 
;; (add-hook 'c-ts-mode-hook  #'ts-fold-mode)
;; (add-hook 'c++-ts-mode-hook #'ts-fold-mode)
;; 
