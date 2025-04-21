;;; treesitter-setup.el --- Tree‑sitter, ts-fold & line‑numbers  -*- lexical-binding: t; -*-
;; Load *early* in init so that mode‑remapping happens before files open.
;; -----------------------------------------------------------------------------

;;;; Requirements --------------------------------------------------------------
(require 'treesit)

(use-package treesit-auto
  :straight (treesit-auto :host github :repo "renzmann/treesit-auto")
  :demand t
  :custom
  ;; If a grammar is missing, prompt once and compile it with `treesit-auto`.
  (treesit-auto-install 'prompt)
  :config
  ;; Languages you actually care about. Extend as needed.
  (setq treesit-language-source-alist
        '((bash          "https://github.com/tree-sitter/tree-sitter-bash")
          (c             "https://github.com/tree-sitter/tree-sitter-c")
          (cpp           "https://github.com/tree-sitter/tree-sitter-cpp")
          (c_sharp       "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (css           "https://github.com/tree-sitter/tree-sitter-css")
          (go            "https://github.com/tree-sitter/tree-sitter-go")
          (html          "https://github.com/tree-sitter/tree-sitter-html")
          (javascript    "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json          "https://github.com/tree-sitter/tree-sitter-json")
          (lua           "https://github.com/Azganoth/tree-sitter-lua")
          (markdown      "https://github.com/ikatyang/tree-sitter-markdown")
          (python        "https://github.com/tree-sitter/tree-sitter-python")
          (rust          "https://github.com/tree-sitter/tree-sitter-rust")
          (typescript    "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx           "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (toml          "https://github.com/tree-sitter/tree-sitter-toml")
          (yaml          "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; Helper to (re)compile grammars for *all* the languages above.
  (defun my/treesit-install-all-languages ()
    "Install every grammar listed in `treesit-language-source-alist`."
    (interactive)
    (dolist (lang (mapcar #'car treesit-language-source-alist))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang))))

  ;; Prefer ts‑modes everywhere grammars exist.
  (setq major-mode-remap-alist
        '((bash-mode        . bash-ts-mode)
          (c-mode           . c-ts-mode)
          (c++-mode         . c++-ts-mode)
          (csharp-mode      . csharp-ts-mode)
          (css-mode         . css-ts-mode)
          (go-mode          . go-ts-mode)
          (html-mode        . html-ts-mode)
          (javascript-mode  . js-ts-mode)
          (js-mode          . js-ts-mode)
          (json-mode        . json-ts-mode)
          (lua-mode         . lua-ts-mode)
          (markdown-mode    . markdown-ts-mode)
          (python-mode      . python-ts-mode)
          (rust-mode        . rust-ts-mode)
          (typescript-mode  . tsx-ts-mode)
          (yaml-mode        . yaml-ts-mode)))

  ;; Maximum capture detail (0–4). Level 4 gives parameters, fields, etc.
  (setq treesit-font-lock-level 4)

  ;; Some themes forget to style the new generic TS faces.  Safely inherit.
  (dolist (pair '((tree-sitter-hl-face:constant   . font-lock-constant-face)
                  (tree-sitter-hl-face:number     . font-lock-constant-face)
                  (tree-sitter-hl-face:property   . font-lock-variable-name-face)
                  (tree-sitter-hl-face:module     . font-lock-type-face)))
    (when (facep (car pair))                          ; only if the face exists
      (set-face-attribute (car pair) nil :inherit (cdr pair))))

  ;; Finally enable auto‑mode remap globally.
  (global-treesit-auto-mode))

;;;; Folding -------------------------------------------------------------------
(use-package ts-fold
  :straight (ts-fold :host github :repo "emacs-tree-sitter/ts-fold")
  :hook ((prog-mode . ts-fold-indicators-mode)
         (prog-mode . ts-fold-mode))
  :custom
  (ts-fold-line-count-show nil)
  (ts-fold-replacement " ▶ … "))

;;;; Line‑numbers (relative, fold‑aware) ---------------------------------------
(defun my/display-line-numbers--relative (line)
  "Return RELATIVE line number or empty string for folded (invisible) LINE."
  (let* ((pos (save-excursion
                (goto-char (line-beginning-position))
                (forward-line (- line (line-number-at-pos)))
                (point))))
    (unless (invisible-p pos)
      (number-to-string (abs (- line (line-number-at-pos)))))))

(setq display-line-numbers-type 'relative
      display-line-numbers-function #'my/display-line-numbers--relative)
(global-display-line-numbers-mode 1)

;;;; Org‑mode fallback ---------------------------------------------------------
;; Org currently has no built‑in TS grammar, so rely on classic font‑lock.
(add-hook 'org-mode-hook #'turn-on-font-lock)

(provide 'treesitter-setup)
;;; treesitter-setup.el ends here
;;; lsp-setup.el --- LSP settings kept separate from treesitter  -*- lexical-binding: t; -*-
;; Load *after* your completion framework but before language buffers open.
;; -----------------------------------------------------------------------------

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; By default we turn semantic tokens *off* because you prefer Tree‑sitter’s
  ;; colouring.  Toggle with \[lsp-semantic-tokens-mode] in any buffer.
  (lsp-semantic-tokens-enable nil)
  (lsp-enable-snippet t)
  (lsp-idle-delay 0.2)
  (lsp-prefer-capf t)
  :config
  ;; Standard per‑language hooks (only ts‑modes listed here).
  (dolist (hook '(c-ts-mode-hook
                  c++-ts-mode-hook
                  python-ts-mode-hook
                  rust-ts-mode-hook
                  lua-ts-mode-hook
                  js-ts-mode-hook
                  tsx-ts-mode-hook))
    (add-hook hook #'lsp-deferred))

  ;; Optional: fine‑tune semantic token faces *if* you decide to turn them on.
  (setq lsp-semantic-token-face-overrides
        '((variable     . font-lock-variable-name-face)
          (parameter    . font-lock-variable-name-face)
          (property     . font-lock-variable-name-face)
          (function     . font-lock-function-name-face)
          (method       . font-lock-function-name-face)
          (namespace    . font-lock-type-face))))

(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-show-code-actions t))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
