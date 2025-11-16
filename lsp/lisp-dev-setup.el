;;; lisp-dev-setup.el --- Complete Common Lisp Development Environment -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified configuration for Common Lisp development with SLY (REPL/evaluation),
;; LSP (code intelligence), and Flycheck (diagnostics)

;;; Code:

;; First, ensure Quicklisp is installed
(defvar my/quicklisp-path
  (expand-file-name "~/quicklisp/")
  "Path to Quicklisp installation.")

(defun my/ensure-quicklisp ()
  "Ensure Quicklisp is installed."
  (unless (file-exists-p (expand-file-name "setup.lisp" my/quicklisp-path))
    (message "Quicklisp not found. Please install it manually:")
    (message "1. Download: curl -O https://beta.quicklisp.org/quicklisp.lisp")
    (message "2. In SBCL: sbcl --load quicklisp.lisp")
    (message "3. Run: (quicklisp-quickstart:install)")
    (message "4. Run: (ql:add-to-init-file)")
    nil))

;; Remove any existing SLIME hooks
(when (member 'slime-mode (default-value 'lisp-mode-hook))
  (remove-hook 'lisp-mode-hook 'slime-mode))

;; Configure SLY for REPL and evaluation
;; Configure SLY
(use-package sly
  :ensure t
  :defer t
  :hook (lisp-mode . sly-mode)
  :custom
  (inferior-lisp-program "sbcl")
  (sly-complete-symbol-function 'sly-flex-completions)
  (sly-net-coding-system 'utf-8-unix)
  :config
  ;; IMPORTANT: Set contribs BEFORE calling sly-setup
  (setq sly-contribs '(sly-fancy sly-mrepl sly-trace-dialog))
  
  ;; Auto-detect Quicklisp
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "4096")
                :coding-system utf-8-unix)
          (ccl ("ccl") :coding-system utf-8-unix)))
  
  ;; Now setup SLY
  (sly-setup)
  
  ;; Add SLY's completion to Corfu
  (with-eval-after-load 'cape
    (add-to-list 'completion-at-point-functions #'sly-complete-at-point)))

;; SLY extensions
(use-package sly-quicklisp
  :ensure t
  :defer t
  :after sly)

(use-package sly-asdf
  :ensure t
  :defer t
  :after sly)

(use-package sly-macrostep
  :ensure t
  :defer t
  :after sly)

;; Better syntax highlighting
(use-package lisp-extra-font-lock
  :ensure t
  :defer t
  :hook ((lisp-mode common-lisp-mode) . lisp-extra-font-lock-mode))

;; Rainbow delimiters for parenthesis matching
;; (use-package rainbow-delimiters
;;   :ensure t
;;   :hook ((lisp-mode common-lisp-mode sly-mrepl-mode) . rainbow-delimiters-mode))

;; Cape for better completion integration
(use-package cape
  :ensure t
  :defer t
  :after corfu)

;; Configure LSP for Common Lisp
(defun my/setup-lisp-lsp ()
  "Setup LSP for Common Lisp with cl-lsp."
  ;; Ensure cl-lsp is available
  (if (file-exists-p (expand-file-name "~/.roswell/bin/cl-lsp"))
      (progn
        ;; Only start LSP if not in REPL buffer and alive-lsp is disabled
        (unless (or (string-match-p "\\*sly-mrepl" (buffer-name))
                    (member 'alive-lsp lsp-enabled-clients))
          (lsp-deferred)))
    (message "cl-lsp not found. Install with: ros install lem-project/lem-language-server")))

;; Master lisp-mode setup function
(defun my/lisp-mode-setup ()
  "Setup function for lisp-mode with SLY and LSP."
  ;; Basic setup
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local lisp-indent-function 'common-lisp-indent-function)
  
  ;; Enable electric pair mode for parentheses
  (electric-pair-local-mode 1)
  (setq-local electric-pair-pairs '((?\( . ?\))
                                     (?\[ . ?\])
                                     (?\{ . ?\})
                                     (?\" . ?\")))
  
  ;; Enable show-paren-mode
  (show-paren-local-mode 1)
  
  ;; Start SLY mode (for REPL interaction)
  (sly-mode 1)
  
  ;; Start LSP (for code intelligence)
  ;; LSP and SLY can coexist - SLY handles REPL, LSP handles code analysis
  (my/setup-lisp-lsp)
  
  ;; Setup completion
  (corfu-mode 1)
  
  ;; Font lock
  (font-lock-mode 1)
  (font-lock-ensure))

;; Hook our setup to lisp-mode
(add-hook 'lisp-mode-hook #'my/lisp-mode-setup)

;; Setup for SLY REPL
(defun my/sly-mrepl-setup ()
  "Setup SLY MREPL with proper completion and highlighting."
  (lisp-extra-font-lock-mode 1)
  ;;(rainbow-delimiters-mode 1)
  (corfu-mode 1)
  (electric-pair-local-mode 1)
  (show-paren-local-mode 1))

(add-hook 'sly-mrepl-mode-hook #'my/sly-mrepl-setup)

;; Configure LSP mode settings for Common Lisp
;; Configure LSP mode settings for Common Lisp
(with-eval-after-load 'lsp-mode
  ;; IMPORTANT: Disable the built-in alive-lsp client that uses TCP
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda () '("~/.roswell/bin/cl-lsp" "stdio")))
    :major-modes '(lisp-mode)
    :server-id 'cl-lsp
    :priority 1))  ;; Higher priority than alive-lsp (-1)
  
  ;; Remove alive-lsp from the clients if it's loaded
  (setq lsp-disabled-clients '(alive-lsp))
  
  ;; Register Common Lisp with LSP
  (add-to-list 'lsp-language-id-configuration '(lisp-mode . "common-lisp"))
  
  ;; Set cl-lsp specific settings
  (setq lsp-clients-common-lisp-enabled nil)  ;; Disable built-in client
  
  ;; Update Quicklisp path if it exists
  (when (my/ensure-quicklisp)
    (setq lsp-common-lisp-load-paths 
          (list my/quicklisp-path
                (expand-file-name "local-projects/" my/quicklisp-path)))))
  
  ;; Set cl-lsp specific settings
  (setq lsp-clients-common-lisp-enabled t)
  
  ;; Update Quicklisp path if it exists
  (when (my/ensure-quicklisp)
    (setq lsp-common-lisp-load-paths 
          (list my/quicklisp-path
                (expand-file-name "local-projects/" my/quicklisp-path))))

;; Flycheck integration for Common Lisp
(with-eval-after-load 'flycheck
  ;; Define a custom checker that uses SLY's compilation results
  (flycheck-define-generic-checker 'sly-checker
    "A syntax checker for Common Lisp using SLY."
    :start (lambda (checker callback)
             (if (and (bound-and-true-p sly-mode)
                      sly-buffer-connection)
                 (funcall callback 'finished nil)
               (funcall callback 'finished nil)))
    :modes '(lisp-mode)
    :predicate (lambda () (bound-and-true-p sly-mode)))
  
  ;; Add to checkers list
  (add-to-list 'flycheck-checkers 'sly-checker))

;; Evil mode integration
(with-eval-after-load 'evil
  (with-eval-after-load 'sly
    ;; Add SLY REPL mode to evil collection
    (when (boundp 'evil-collection-mode-list)
      (add-to-list 'evil-collection-mode-list 'sly)
      (add-to-list 'evil-collection-mode-list 'sly-mrepl))))

;; Keybindings for Common Lisp development
(with-eval-after-load 'general
  (general-define-key
   :keymaps 'lisp-mode-map
   :states '(normal visual)
   :prefix "SPC m"
   "" '(:ignore t :which-key "lisp")
   
   ;; REPL
   "'" '(sly :which-key "start/switch to repl")
   "r" '(:ignore t :which-key "repl")
   "rc" '(sly-connect :which-key "connect")
   "rr" '(sly-restart-inferior-lisp :which-key "restart")
   "rq" '(sly-quit-lisp :which-key "quit")
   
   ;; Evaluation
   "e" '(:ignore t :which-key "eval")
   "eb" '(sly-eval-buffer :which-key "buffer")
   "ee" '(sly-eval-last-expression :which-key "last expr")
   "ef" '(sly-eval-defun :which-key "defun")
   "er" '(sly-eval-region :which-key "region")
   "el" '(sly-eval-print-last-expression :which-key "print last")
   
   ;; Compilation
   "c" '(:ignore t :which-key "compile")
   "cc" '(sly-compile-file :which-key "file")
   "cC" '(sly-compile-and-load-file :which-key "file and load")
   "cf" '(sly-compile-defun :which-key "defun")
   "cr" '(sly-compile-region :which-key "region")
   
   ;; Navigation (using LSP)
   "g" '(:ignore t :which-key "goto")
   "gg" '(lsp-find-definition :which-key "definition")
   "gr" '(lsp-find-references :which-key "references")
   "gi" '(lsp-find-implementation :which-key "implementation")
   "gt" '(lsp-find-type-definition :which-key "type definition")
   
   ;; Documentation
   "h" '(:ignore t :which-key "help")
   "hh" '(sly-describe-symbol :which-key "describe")
   "ha" '(sly-apropos :which-key "apropos")
   "hd" '(sly-documentation-lookup :which-key "documentation")
   "hH" '(sly-hyperspec-lookup :which-key "hyperspec")
   
   ;; Debugging
   "d" '(:ignore t :which-key "debug")
   "db" '(sly-toggle-break-on-signals :which-key "break on signals")
   "dt" '(sly-toggle-trace-fdefinition :which-key "trace")
   "dT" '(sly-untrace-all :which-key "untrace all")
   
   ;; Macros
   "m" '(:ignore t :which-key "macro")
   "me" '(sly-expand-1 :which-key "expand-1")
   "mE" '(sly-macroexpand-all :which-key "expand all")
   "ms" '(sly-macrostep-expand :which-key "macrostep")
   
   ;; LSP specific
   "l" '(:ignore t :which-key "lsp")
   "la" '(lsp-execute-code-action :which-key "code action")
   "lf" '(lsp-format-buffer :which-key "format")
   "lr" '(lsp-rename :which-key "rename")
   "ls" '(lsp-signature-help :which-key "signature")))

(with-eval-after-load 'sly
  (evil-define-key 'normal sly-mode-map
    (kbd "C-x C-x") #'sly-eval-defun
  ))

;; Helper function to start Common Lisp development
(defun my/start-lisp-dev ()
  "Start Common Lisp development environment with SLY REPL."
  (interactive)
  (unless (sly-connected-p)
    (sly))
  (message "Common Lisp development environment ready!"))

;; Auto-start SLY when opening a Lisp project
(defun my/maybe-start-sly ()
  "Start SLY if in a Lisp project."
  (when (and (eq major-mode 'lisp-mode)
             (not (sly-connected-p))
             (or (locate-dominating-file default-directory "*.asd")
                 (locate-dominating-file default-directory "quicklisp")))
    (sly 'sbcl)))

;; Optional: auto-start SLY for projects
;; (add-hook 'lisp-mode-hook #'my/maybe-start-sly)

(provide 'lisp-dev-setup)
;;; lisp-dev-setup.el ends here
