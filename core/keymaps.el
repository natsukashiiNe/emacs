(defvar my/evil-override-maps
  '(evil-normal-state-map
    help-mode-map
    xref--xref-buffer-mode-map
    compilation-mode-map
    occur-mode-map
    special-mode-map
    magit-status-mode-map
    dired-mode-map)
  "A list of keymaps where evil-style keybindings should apply.")

(defvar my/spc-prefix nil
  "A placeholder for the 'SPC' prefix command.")

;; Define the prefix command once, so we can reuse it in all maps.
(define-prefix-command 'my/spc-prefix)

(defun my/register-spc-prefixes ()
  "Define 'SPC' as a prefix in each keymap from `my/evil-override-maps`."
  (dolist (map-sym my/evil-override-maps)
    (let ((km (symbol-value map-sym)))
      (unless (keymapp km)
        (error "Not a keymap: %S" map-sym))
      (define-key km (kbd "SPC") 'my/spc-prefix))))

(my/register-spc-prefixes) 

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC j"
 :ru_key "SPC о"
 :command nil
 :desc "lsp commands")

                                        ; no prefix
(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-h"
 :command #'evil-window-left
 :desc "Focus right window")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-j"
 :command #'evil-window-bottom
 :desc "Focus right window")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-k"
 :command #'evil-window-up
 :desc "Focus right window")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-l"
 :command #'evil-window-right
 :desc "Focus right window")

;; Keep capital "C-H" for help
(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-S-h"
 :command #'help-command
 :desc "Help")

;; clang magic
(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j h"
 :command 'lsp-clangd-find-other-file 
 :desc "[h]eader-impl switch")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j l"
 :command 'lsp-find-references 
 :desc "find [l]inks(refs)")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j r"
 :command 'lsp-rename
 :desc "smart [r]ename")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j s"
 :command 'lsp-find-declaration 
 :desc "goto [s]ignature")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j d"
 :command 'lsp-find-definition 
 :desc "goto [d]efinition")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j m"
 :command 'lsp-find-implementation
 :desc "goto i[m]plementation")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j w"
 :command 'lsp-ui-doc-toggle 
 :desc "toggle help")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j t"
 :command 'lsp-find-type-definition 
 :desc "[t]ype definiton")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j c"
 :command 'consult-flymake 
 :desc "[c]onsult flymake (diag.)")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j p"
 :command 'flymake-show-project-diagnostics 
 :desc "flymake of the [p]roject")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j b"
 :command 'flymake-show-buffer-diagnostics 
 :desc "flymake of the [b]uffer")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC j j"
 :command 'flymake-goto-next-error 
 :desc "flymake next")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
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
 :maps my/evil-override-maps
 :key "SPC h"
 :command nil ;; prefix
 :desc "get completions")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC H"
 :command nil ;; prefix
 :desc "get help")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC h e"
 :command (lambda () (interactive) (my/exec-with-prefix "evil- "))
 :desc "[e]vil commands")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC h h"
 :command (lambda () (interactive) (my/exec-with-prefix "describe- "))
 :desc "describe")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC h c"
 :command (lambda () (interactive) (my/exec-with-prefix "projectile- "))
 :desc "[c]onsult commands")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC h p"
 :command (lambda () (interactive) (my/exec-with-prefix "projectile- "))
 :desc "[p]rojectile commands")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC h n"
 :command (lambda () (interactive) (my/exec-with-prefix "persp- "))
 :desc "perspective commands")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC h b"
 :command (lambda () (interactive) (my/exec-with-prefix "eyebrowse- "))
 :desc "eye[b]rowse commands")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC h d"
 :command (lambda () (interactive) (my/exec-with-prefix "dired- "))
 :desc "dired commands")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC h l"
 :command (lambda () (interactive) (my/exec-with-prefix "lsp- "))
 :desc "[l]sp commands")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC h f"
 :command (lambda () (interactive) (my/exec-with-prefix "treemacs- "))
 :desc "treemacs commands")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC h f"
 :command 'describe-function
 :desc "treemacs commands")


;; BUFFER / PROJECT NAVIGATON

;; TODO:  probably that will be needed to count only for "pesp-session" buffer
(keymap-set global-map "C-S-O" 'projectile-previous-project-buffer)
(keymap-set global-map "C-S-I" 'projectile-next-project-buffer)

;; Tab Navigation
;; magit-diff-mode

;; (keymap-set magit-hunk-section-map "M-w" 'tab-bar-switch-to-next-tab)
;; (keymap-set magit-hunk-section-map "M-W" 'tab-bar-switch-to-prev-tab)
(keymap-set global-map "M-w" 'tab-bar-switch-to-next-tab)
(keymap-set global-map "M-W" 'tab-bar-switch-to-prev-tab)

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC f"
 :command nil ;; prefix
 :desc "search")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC f f"
 :command 'projectile-find-file
 :desc "projectile-find-file")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC f j"
 :command 'consult-project-buffer
 :desc "consult-project-buffer")  

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC f s"
 :command 'projectile-grep
 :desc "projectile grep")  

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC f s"
 :command 'projectile-grep
 :desc "projectile grep")  

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC f m"
 :command 'imenu
 :desc "imenu")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC f e"
 :command 'projectile-dired
 :desc "imenu")

;; TODO compile errors


(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-n"
 :command nil ;; prefix
 :desc "perspectives")
(keymap-set evil-normal-state-map "C-n" (make-sparse-keymap))

;; perspective
(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-n C-n"
 :command 'persp-switch
 :desc "pesps switch")  

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-n t"
 :command 'tab-switch
 :desc "[t]ab switch")  

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-n C-s"
 :command 'persp-save
 :desc "pesps save")  

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-n C-e"
 :command 'persp-switch-last
 :desc "pesps switch last")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-n e"
 :command 'eyebrowse-last-window-config
 :desc "eye last win conf")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-n n"
 :command 'eyebrowse-switch-to-window-config
 :desc "eye switch")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "C-n w"
 :command 'eyebrowse-rename-window-config
 :desc "eye [w]rite config")


;; evil-snipe
(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "f"
 :command 'evil-snipe-f
 :desc "evil-snipe-f")  


;; ORG mode hotkeys
(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC s"
 :command nil
 :desc "[s]urrond commands")  

;; net(keymap-set-with-desc
;;  :maps '(evil-normal-state-map)
;;  :key "SPC s b"
;;  :command viWS*
;;  :desc "surround [b]old")  
;; 
;; (keymap-set-with-desc
;;  :maps '(evil-normal-state-map)
;;  :key "SPC s B"
;;  :command vaWS*
;;  :desc "surround [b]old (a)")  

;; QUICK FILE ACCESS
(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC F"
 :command nil ;; prefix
 :desc "open files")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC F d"
 :command nil ;; prefix
 :desc "dotfiles")

(keymap-set-with-desc
 :maps my/evil-override-maps
 :key "SPC F d e"
 :command (lambda () (interactive) (find-file "~/.config/emacs/config.org"))
 :desc "emacs/config.org")

;; EVIL INSERT
(keymap-set-with-desc
 :maps '(evil-insert-state-map)
 :key "C-a"
 :command 'move-beggining-of-line
 :desc "beggining of the line")

(keymap-set-with-desc
 :maps '(evil-insert-state-map)
 :key "C-h"
 :command 'evil-delete-backward-char
 :desc "backspace")

(keymap-set-with-desc
 :maps '(evil-insert-state-map)
 :key "C-e"
 :command 'evil-scroll-line-down
 :desc "backspace")

(keymap-set-with-desc
 :maps '(evil-insert-state-map)
 :key "C-S-v"
 :command 'evil-paste-after
 :desc "paste")

;; operations on buffer
(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC b"
 :command nil
 :desc "[b]uffer commands")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC b e"
 :command 'eval-buffer
 :desc "[b]uffer [e]val")

;; SYSTEM
(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC S"
 :command nil
 :desc "[s]ystem commands")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC S f"
 :command 'list-faces-display
 :desc "list [f]aces")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC u"
 :command 'nil
 :desc "[u]i")

(keymap-set-with-desc
 :maps '(evil-normal-state-map)
 :key "SPC u e"
 :command 'treemacs
 :desc "togle tr[e]emacs")

;; (keymap-set-with-desc
;;  :maps '(evil-normal-state-map)
;;  :key "SPC S r"
;;  :command '
;;  :desc "reload config")

;;  MINIBUFFER
(keymap-set minibuffer-local-map "C-w" 'backward-kill-word)
(keymap-set minibuffer-local-map "C-h" 'evil-delete-backward-char)
(keymap-set minibuffer-local-map "C-S-v" 'evil-paste-before)
(keymap-set minibuffer-local-map "<escape>" 'abort-recursive-edit)
