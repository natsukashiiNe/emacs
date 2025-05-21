;; GET COMMANDS FOR THE PLUGINS
(defun my/exec-with-prefix (prefix)
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert prefix))
    (command-execute #'execute-extended-command)))

(use-package general
  :after evil
  :config

  ;; UNPREFIXED
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   "M-C-l" 'tab-bar-switch-to-next-tab
   "M-C-h" 'tab-bar-switch-to-prev-tab
   "C-S-h" 'evil-window-left
   "C-S-j" 'evil-window-bottom
   "C-S-k" 'evil-window-up
   "C-S-l" 'evil-window-right
   )

  
  ;; C-n prefixed
  (general-create-definer my-c-n-leader
    :states '(normal visual)
    :keymaps 'override
    :prefix "C-j")
  (my-c-n-leader
    "" '(:ignore t :which-key "perspectives")
    "C-j" '(persp-switch :which-key "persp-switch")
    "t"   '(tab-switch   :which-key "switch tab")
    "C-s" '(persp-save   :which-key "save perspective")
    "C-e" '(persp-switch-last :which-key "switch last")
    "e"   '(eyebrowse-last-window-config :which-key "eyebrowse last config")
    "i"   '(eyebrowse-switch-to-window-config :which-key "eyebrowse switch config")
    "w"   '(eyebrowse-rename-window-config :which-key "rename config")
    "f"   '(:ignore t :which-key "projectile managment")
    "f o"   '(projectile-switch-project :which-key "projectle switch project"))


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
    "f m" '(consult-imenu :which-key "imenu")
    "f s" '(consult-lsp-file-symbols :which-key "lsp symbols")
    "f S" '(consult-lsp-symbols :which-key "lsp symbols")
    "f e" '(projectile-dired :which-key "dired in project")
    "f i" '(next-buffer     :which-key "next buffer")
    "f o" '(previous-buffer :which-key "previous buffer")

    ;; “SPC t” tab managment
    "t"   '(:ignore t :which-key "tabs managment")
    "t i"   '(tab-new-to :which-key "new tab")
    "t r"   '(tab-close :which-key "close tab")

    ;; "SPC g" for git
    "g"   '(:ignore t :which-key "git managment")
    "g g"   '(magit :which-key "magit")

    ;; “SPC h” prefix for help or “completions”
    "h"   '(:ignore t :which-key "help / completions")
    "h e" '((lambda () (interactive) (my/exec-with-prefix "evil- "))
            :which-key "[e]vil commands")
    "h C" '((lambda () (interactive) (my/exec-with-prefix "evilnc- "))
            :which-key "evil [c]omments commands")
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
  )


;; mode-specific maps:
(keymap-set xref--xref-buffer-mode-map "S-j" 'xref-next-line)
(keymap-set xref--xref-buffer-mode-map "S-k" 'xref-prev-line)

;; MINIBUFFER
(keymap-set minibuffer-local-map "C-w" 'backward-kill-word)
(keymap-set minibuffer-local-map "C-h" 'evil-delete-char)
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

;; VTERM
(keymap-set  vterm-mode "C-M-l" 'tab-bar-switch-to-next-tab)
(keymap-set  vterm-mode "M-C-h" 'tab-bar-switch-to-prev-tab)
