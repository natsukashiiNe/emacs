(defun my/exec-with-prefix (prefix)
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert prefix))
    (command-execute #'execute-extended-command)))

(use-package general
  :after evil
  :demand t
  :config

  (add-hook
   'evil-mode-hook
   (lambda ()
     ;; reapply evil-mode (needs to properly hook general)
     (message "Applying general keybinds!")

     ;; UNPREFIXED
     (general-define-key
      :states '(normal visual insert)
      :keymaps 'override
      "M-l" 'tab-bar-switch-to-next-tab
      "M-h" 'tab-bar-switch-to-prev-tab
      "C-S-h" 'evil-window-left
      "C-S-j" 'evil-window-bottom
      "C-S-k" 'evil-window-up
      "C-S-l" 'evil-window-right
      )

     ;; Motioned
     (general-define-key
      :states  '(normal motion)
      :keymaps 'override
      "f" #'avy-goto-line
      "s" #'avy-goto-char-2
      "F" #'evil-avy-goto-char-in-line
      "W" #'avy-goto-word-0
      )

     
     ;; C-n prefixed
     (general-create-definer my-c-j-leader
       :states '(normal visual)
       :keymaps 'override
       :prefix "C-j")
     (my-c-j-leader
       "" '(:ignore t :which-key "Navigation")
       ;; Perspective
       "C-j" '(persp-switch :which-key "persp-switch")
       ;; Tabs
       "C-o" '(tab-switch   :which-key "switch tab")
       "i"   '(tab-bar-switch-to-recent-tab   :which-key "recent tab")
       "e"   '(projectile-previous-project-buffer   :which-key "recent buffer")
       "C-s" '(persp-save   :which-key "save perspective")
       "C-e" '(persp-switch-last :which-key "switch last")
       ;; TODO eyebrowse
       ;;"e"   '(eyebrowse-last-window-config :which-key "eyebrowse last config")
       ;;"i"   '(eyebrowse-switch-to-window-config :which-key "eyebrowse switch config")
       ;;"w"   '(eyebrowse-rename-window-config :which-key "rename config")
       "C-h"   '(:ignore t :which-key "projectile managment")
       "C-h C-h"   '(projectile-switch-project :which-key "projectle switch project"))


     ;; SPC prefix
     (general-create-definer my-leader
       :states '(normal visual)
       :keymaps 'override
       :prefix "SPC"
       :non-normal-prefix "M-SPC")

     (my-leader
       ;; “SPC f” prefix for “search” or “files”
       "f"   '(:ignore t :which-key "search / files")
       "f f" '(projectile-find-file :which-key "projectile-find-file")
       "f j" '(consult-project-buffer :which-key "consult-project-buffer")
       "f g" '(consult-grep :which-key "projectile grep")
       "f G" '(projectile-grep :which-key "projectile grep")
       "f s" '(consult-lsp-file-symbols :which-key "lsp symbols")
       "f S" '(consult-lsp-symbols :which-key "lsp symbols")
       "f e" '(projectile-dired :which-key "dired in project")
       "f i" '(next-buffer     :which-key "next buffer")
       "f o" '(previous-buffer :which-key "previous buffer")

       ;;"f m" '(lambda ()
       ;;         (interactive)
       ;;         (cond
       ;;          ((derived-mode-p 'org-mode) (call-interactively #'consult-org-heading))
       ;;          (t (call-interactively #'consult-lsp-file-symbols)))
       ;;         :which-key "consult [m]enu")
       ;; 

       ;; “SPC t” tab managment
       "t"   '(:ignore t :which-key "tabs managment")
       "t i"   '(tab-new-to :which-key "new tab")
       "t r"   '(tab-close :which-key "close tab")

       ;; "SPC g" for git
       "g"   '(:ignore t :which-key "git managment")
       "g g"   '(magit :which-key "magit")

       ;; "SPC c" for consult


       ;; “SPC h” prefix for help or “completions”
       "h"   '(:ignore t :which-key "help / completions")
       "h e" '((lambda () (interactive) (my/exec-with-prefix "evil- "))
               :which-key "[e]vil commands")
       "h C" '((lambda () (interactive) (my/exec-with-prefix "evilnc- "))
               :which-key "evil [c]omment commands")
       "h h" '((lambda () (interactive) (my/exec-with-prefix "describe- "))
               :which-key "describe")
       "h c" '((lambda () (interactive) (my/exec-with-prefix "consult- "))
               :which-key "[c]onsult commands")
       "h p" '((lambda () (interactive) (my/exec-with-prefix "projectile- "))
               :which-key "[p]rojectile commands")
       "h b" '((lambda () (interactive) (my/exec-with-prefix "eyebrowse- "))
               :which-key "eye[b]rowse commands")
       "h d" '((lambda () (interactive) (my/exec-with-prefix "dired - "))
               :which-key "[d]ired commands")
       "h l" '((lambda () (interactive) (my/exec-with-prefix "lsp - "))
               :which-key "[l]sp commands")
       "h t" '((lambda () (interactive) (my/exec-with-prefix "treemacs - "))
               :which-key "treemacs commands")
       "h n" '((lambda () (interactive) (my/exec-with-prefix "persp - "))
               :which-key "persp commands")

       ;; “SPC j” for LSP / Flymake
       "j"   '(:ignore t :which-key "lsp / flymake")
       "j h" '(lsp-clangd-find-other-file        :which-key "switch .h/.cpp")
       "j l" '(lsp-find-references               :which-key "find references")
       "j r" '(lsp-rename                        :which-key "smart [r]ename")
       "j s" '(lsp-signature                     :which-key "lsp [s]ignature")
       "j d" '(lsp-find-definition               :which-key "goto definition")
       "j m" '(lsp-find-implementation           :which-key "goto i[m]pl.")
       "j w" '(lsp-ui-doc-toggle                 :which-key "toggle doc")
       "j t" '(lsp-find-type-definition          :which-key "[t]ype definition")
       "j c" '(consult-flymake                   :which-key "consult flymake")
       "j p" '(flymake-show-project-diagnostics  :which-key "flymake [p]roject")
       "j j" '(flymake-goto-next-error           :which-key "next error")
       "j k" '(flymake-goto-prev-error           :which-key "prev error")

       ;; “SPC b” for buffer operations
       "b"   '(:ignore t :which-key "buffer")
       "b e" '(eval-buffer :which-key "evaluate buffer")

       ;; “SPC S” for system ops
       "S"   '(:ignore t :which-key "system")
       "S f" '(list-faces-display :which-key "list faces")

       ;; interface
       "u e" '(treemacs :which-key "treemacs")
       )
     )))




;; =============================== EVIL NORMAL KEYMAPS ===============================

;; ============================== MODE SPECIFIC KEYMAPS ==============================
;; xref

(with-eval-after-load 'xref
  (evil-define-key 'normal xref--xref-buffer-mode-map
    (kbd "S-j") #'xref-next-line
    (kbd "S-k") #'xref-prev-line))

;; vdiff
(with-eval-after-load 'vdiff
  (evil-define-key 'normal vdiff-mode-map
    (kbd "S-j") #'vdiff-next-hunk
    (kbd "S-k") #'vdiff-previous-hunk))

(with-eval-after-load 'magit
  (evil-define-key 'normal magit-status-mode
    (kbd "M-s M-s") #'magit-stage)
  (evil-define-key 'normal magit-stash-mode-map
    (kbd "e") #'vdiff-magit-dwim
    (kbd "E") #'vdiff-magit)
  (evil-define-key 'normal magit-revision-mode-map
    (kbd "e") #'vdiff-magit-dwim
    (kbd "E") #'vdiff-magit)
  (evil-define-key 'normal magit-commit-section-map
    (kbd "s") #'vdiff-magit-dwim))

(with-eval-after-load 'embark
  (evil-define-key 'normal embark-collect-mode-map
    (kbd "K") #'outline-previous-heading
    (kbd "J") #'outline-next-heading
    (kbd "H") #'outline-hide-body
    (kbd "L") #'outline-show-all))

(with-eval-after-load 'flycheck
  (evil-define-key 'normal flycheck-mode-map
    (kbd "M-w M-f") #'consult-flycheck))

(with-eval-after-load 'flyspell
  (evil-define-key 'normal org-mode-map
    (kbd "M-w M-f") #'consult-flyspell))

(evil-define-key 'normal org-mode-map
  (kbd "M-w i") #'consult-org-heading)

(define-prefix-command 'my/meta-w-prefix)
(keymap-global-set "M-w" 'my/meta-w-prefix)   ;; M-w now opens that map
(keymap-set my/meta-w-prefix "M-w" #'projectile-find-file)
(keymap-set my/meta-w-prefix "M-g" #'consult-grep)
(keymap-set my/meta-w-prefix "M-j" #'consult-project-buffer)
(keymap-set my/meta-w-prefix "g" #'consult-grep)
(keymap-set my/meta-w-prefix "j" #'consult-project-buffer)


(keymap-set my/meta-w-prefix "s" #'consult-lsp-file-symbols)
;; (keymap-set my/meta-w-prefix "i" #'consult-imenu)


;; Lsp-based buffers
;; (defvar my-lsp-leader-map
;;   (define-keymap
;;     "h" #'lsp-clangd-find-other-file
;;     "l" #'lsp-find-references
;;     "r" #'lsp-rename
;;     "s" #'lsp-signature
;;     "d" #'lsp-find-definition
;;     "m" #'lsp-find-implementation
;;     "w" #'lsp-ui-doc-toggle
;;
;;     "c" #'consult-flymake
;;     "p" #'flymake-show-project-diagnostics
;;     "j" #'flymake-goto-next-error
;;     "k" #'flymake-goto-prev-error)
;;   "Leader keymap for LSP")
;; 
;; ;; Bind SPC j globally in evil normal state
;; (keymap-set evil-normal-state-map "SPC j" my-lsp-leader-map)

;; ORG MODE KEYMAPS



;; MINIBUFFER
(keymap-set minibuffer-local-map "C-w" 'backward-kill-word)
(keymap-set minibuffer-local-map "C-h" 'backward-delete-char)
(keymap-set minibuffer-local-map "C-S-v" 'evil-paste-before)
(keymap-set minibuffer-local-map "<escape>" 'abort-recursive-edit)
(keymap-set minibuffer-local-map
            "C-S-o"
            (lambda ()
              (interactive)
              (my/set-vertico-count
               (+ vertico-count 10))))

(keymap-set minibuffer-local-map
            "C-S-w"
            (lambda ()
              (interactive)
              (my/set-vertico-count
               (max 1 (- vertico-count 10)))))

;; evil insert
(keymap-set evil-insert-state-map "C-h" 'evil-delete-backward-char)
;; evil normal
(keymap-set evil-normal-state-map "C-x C-h" 'consult-org-heading)

;; VTERM
;;(keymap-set  vterm-mode "C-M-l" 'tab-bar-switch-to-next-tab)
;;(keymap-set  vterm-mode "M-C-h" 'tab-bar-switch-to-prev-tab)

;; Toggling to hook my general on it
(message "toggling evil")
(evil-mode 0)
(evil-mode 1)


;; (with-eval-after-load 'evil
;;   (define-key minibuffer-local-map (kbd "C-h") #'evil-delete-char)
;;   (define-key minibuffer-local-completion-map (kbd "C-h") #'evil-delete-char)
;;   (define-key minibuffer-local-map (kbd "C-w") #'backward-kill-word)
;;   (define-key minibuffer-local-completion-map (kbd "C-w") #'backward-kill-word)
;;   ;; If needed, also:
;;   ;; (define-key minibuffer-local-ns-map (kbd "C-h") #'backward-kill-word)
;;   ;; (define-key minibuffer-local-isearch-map (kbd "C-h") #'backward-kill-word)
;;   )
;; 
;; (evil-define-key 'insert evil-insert-state-map
;;   (kbd "C-h") #'evil-delete-backward-char)
