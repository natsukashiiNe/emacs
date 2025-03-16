;; LSP COMMANDS
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC j"
 :command nil ;; prefix
 :desc "lsp commands")

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
 :key "SPC t c"
 :command (lambda () (interactive) (my/exec-with-prefix "projectile- "))
 :desc "consult commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t p"
 :command (lambda () (interactive) (my/exec-with-prefix "projectile- "))
 :desc "projectile commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t e"
 :command (lambda () (interactive) (my/exec-with-prefix "eyebrowse- "))
 :desc "eyebrowse commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t d"
 :command (lambda () (interactive) (my/exec-with-prefix "dired- "))
 :desc "dired commands")

(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC t l"
 :command (lambda () (interactive) (my/exec-with-prefix "lsp- "))
 :desc "lsp commands")


;; BUFFER / PROJECT NAVIGATON

;; TODO:  probably that will be needed to count only for "pesp-session" buffer
(keymap-set evil-normal-state-map "C-O" 'previous-buffer) 
(keymap-set evil-normal-state-map "C-I" 'next-buffer)

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


;; SYSTEM
(keymap-set-with-desc
 :map evil-normal-state-map
 :key "SPC S"
 :command nil
 :desc "[s]ystem commands")

;; (keymap-set-with-desc
;;  :map evil-normal-state-map
;;  :key "SPC S r"
;;  :command '
;;  :desc "reload config")
