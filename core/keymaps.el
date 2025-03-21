;; LSP COMMANDS
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j"
 :ru_key "SPC о"
 :command nil ;; prefix
 :desc "lsp commands")

                                        ; no prefix
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "C-h"
 :command #'evil-window-left
 :desc "Focus right window")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "C-j"
 :command #'evil-window-bottom
 :desc "Focus right window")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "C-k"
 :command #'evil-window-up
 :desc "Focus right window")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "C-l"
 :command #'evil-window-right
 :desc "Focus right window")

;; Keep capital "C-H" for help
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "C-S-h"
 :command #'help-command
 :desc "Help")

;; clang magic
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j h"
 :command 'lsp-clangd-find-other-file 
 :desc "[h]eader-impl switch")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j l"
 :command 'lsp-find-references 
 :desc "find [l]inks(refs)")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j r"
 :command 'lsp-rename
 :desc "smart [r]ename")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j s"
 :command 'lsp-find-declaration 
 :desc "goto [s]ignature")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j d"
 :command 'lsp-find-definition 
 :desc "goto [d]efinition")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j m"
 :command 'lsp-find-implementation
 :desc "goto i[m]plementation")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j w"
 :command 'lsp-ui-doc-toggle 
 :desc "toggle help")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j t"
 :command 'lsp-find-type-definition 
 :desc "[t]ype definiton")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j c"
 :command 'consult-flymake 
 :desc "[c]onsult flymake (diag.)")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j p"
 :command 'flymake-show-project-diagnostics 
 :desc "flymake of the [p]roject")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j b"
 :command 'flymake-show-buffer-diagnostics 
 :desc "flymake of the [b]uffer")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j j"
 :command 'flymake-goto-next-error 
 :desc "flymake next")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j k"
 :command 'flymake-goto-prev-error 
 :desc "flymake prev")

;; GET COMMANDS FOR THE PLUGINS
(defun my/exec-with-prefix (prefix)
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert prefix))
    (command-execute #'execute-extended-command)))

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t"
 :command nil ;; prefix
 :desc "get completions")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t e"
 :command (lambda () (interactive) (my/exec-with-prefix "evil- "))
 :desc "[e]vil commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t h"
 :command (lambda () (interactive) (my/exec-with-prefix "describe- "))
 :desc "describe")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t c"
 :command (lambda () (interactive) (my/exec-with-prefix "projectile- "))
 :desc "[c]onsult commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t p"
 :command (lambda () (interactive) (my/exec-with-prefix "projectile- "))
 :desc "[p]rojectile commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t n"
 :command (lambda () (interactive) (my/exec-with-prefix "persp- "))
 :desc "perspective commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t b"
 :command (lambda () (interactive) (my/exec-with-prefix "eyebrowse- "))
 :desc "eye[b]rowse commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t d"
 :command (lambda () (interactive) (my/exec-with-prefix "dired- "))
 :desc "dired commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t l"
 :command (lambda () (interactive) (my/exec-with-prefix "lsp- "))
 :desc "[l]sp commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t f"
 :command (lambda () (interactive) (my/exec-with-prefix "treemacs- "))
 :desc "treemacs commands")


;; BUFFER / PROJECT NAVIGATON

;; TODO:  probably that will be needed to count only for "pesp-session" buffer
(keymap-set evil-normal-state-map "C-O" 'projectile-previous-project-buffer)
(keymap-set evil-normal-state-map "C-I" 'projectile-next-project-buffer)

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC f"
 :command nil ;; prefix
 :desc "search")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC f f"
 :command 'projectile-find-file
 :desc "projectile-find-file")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC f j"
 :command 'consult-project-buffer
 :desc "consult-project-buffer")  

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC f s"
 :command 'projectile-grep
 :desc "projectile grep")  

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC f s"
 :command 'projectile-grep
 :desc "projectile grep")  

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC f m"
 :command 'imenu
 :desc "imenu")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC f e"
 :command 'projectile-dired
 :desc "imenu")

;; TODO compile errors


(keymap-set-with-desc
 :map evil-normal-state-map
 :key "C-n"
 :command nil ;; prefix
 :desc "perspectives")
(keymap-set evil-normal-state-map "C-n" (make-sparse-keymap))

;; perspective
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "C-n C-n"
 :command 'persp-switch
 :desc "pesps switch")  

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "C-n C-e"
 :command 'persp-switch-last
 :desc "pesps switch last")

;; evil-snipe
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "f"
 :command 'evil-snipe-f
 :desc "evil-snipe-f")  

;; QUICK FILE ACCESS
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC F"
 :command nil ;; prefix
 :desc "open files")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC F d"
 :command nil ;; prefix
 :desc "dotfiles")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC F d e"
 :command (lambda () (interactive) (find-file "~/.config/emacs/config.org"))
 :desc "emacs/config.org")

;; EVIL INSERT
(keymap-set-with-desc
 :map evil-insert-state-map
 :key "C-a"
 :command 'move-beggining-of-line
 :desc "beggining of the line")

(keymap-set-with-desc
 :map evil-insert-state-map
 :key "C-h"
 :command 'evil-delete-backward-char
 :desc "backspace")

(keymap-set-with-desc
 :map evil-insert-state-map
 :key "C-e"
 :command 'evil-scroll-line-down
 :desc "backspace")

(keymap-set-with-desc
 :map evil-insert-state-map
 :key "C-S-v"
 :command 'evil-paste-after
 :desc "paste")

;; operations on buffer
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC b"
 :command nil
 :desc "[b]uffer commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC b e"
 :command 'eval-buffer
 :desc "[b]uffer [e]val")

;; SYSTEM
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC S"
 :command nil
 :desc "[s]ystem commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC S f"
 :command 'list-faces-display
 :desc "list [f]aces")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC u"
 :command 'nil
 :desc "[u]i")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC u e"
 :command 'treemacs
 :desc "togle tr[e]emacs")

;; (keymap-set-with-desc
;;  :map evil-normal-state-map
;;  :key "SPC S r"
;;  :command '
;;  :desc "reload config")

;;  MINIBUFFER
(keymap-set minibuffer-local-map "C-w" 'backward-kill-word)
(keymap-set minibuffer-local-map "<escape>" 'abort-recursive-edit)
