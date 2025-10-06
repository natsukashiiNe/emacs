;;; general-setup.el --- Keymap settings -*- lexical-binding: t; -*-


;;; Commentary:
;; Defined keymaps with =General= (global overwrite)
;; and evil-define for specific modes

;;; Code:

(eval-when-compile
  (require 'perspective)
  (require 'projectile)
  (require 'magit)
  (require 'avy)
  (require 'evil)
  (require 'use-package)
  )

(defun my/exec-with-prefix (prefix)
  "Execute `execute-extended-command' with PREFIX pre-inserted in minibuffer."
  (interactive)
  (minibuffer-with-setup-hook
      (lambda () (insert prefix))
    (command-execute #'execute-extended-command)))


(defun my/persp-open-project (persp-name project-root &optional mode)
  "Switch Perspective and Projectile with different opening modes.
Switch to PERSP-NAME (creating it if needed), switch Projectile to PROJECT-ROOT
and open it with MODE.

MODE can be:
  'magit  - Open magit-status (default)
  'Dired  - Open Dired in project root
  'find   - Open projectile-find-file prompt
  'shell  - Open shell in project root
  'eshell - Open eshell in project root
  nil     - Default to magit"
  (interactive "sPerspective name: \nDProject root: ")
  (persp-mode 1)
  (let* ((pr (file-truename (expand-file-name project-root)))
         (mode (or mode 'magit)))
    (unless (file-directory-p pr)
      (user-error "Not a directory: %s" pr))
    
    ;; Switch to perspective first, so buffers/windows land there
    (persp-switch persp-name)
    
    ;; Make sure Projectile knows about the project
    (projectile-add-known-project pr)
    
    ;; Override the default action based on mode
    (let ((projectile-switch-project-action
           (pcase mode
             ('magit  (lambda () (magit-status pr)))
             ('dired  (lambda () (dired pr)))
             ('find   (lambda () (projectile-find-file)))
             ('shell  (lambda () (projectile-run-shell)))
             ('eshell (lambda () (projectile-run-eshell)))
             (_       (lambda () (magit-status pr))))))
      ;; Jump straight to the project and run the action
      (projectile-switch-project-by-name pr))))

(defmacro my/setup-quick-projects (&rest projects)
 "Define both the config variable and all project commands.
Each element in PROJECTS is a plist with :name, :persp, :path, and optional :mode."
  `(progn
     (defvar my/quick-open-projects ',projects
       "Quick-open projects configuration.")
     ,@(mapcar
        (lambda (proj)
          (let* ((name (plist-get proj :name))
                 (persp (plist-get proj :persp))
                 (path (plist-get proj :path))
                 (mode (or (plist-get proj :mode) 'magit))
                 (func-name (intern (format "my/open-%s" name)))
                 (mode-desc (pcase mode
                             ('magit "magit")
                             ('dired "dired")
                             ('find "find-file")
                             ('shell "shell")
                             ('eshell "eshell")
                             (_ "default")))
                 (doc (format "Open %s in a new perspective (%s)." persp mode-desc)))
            `(defun ,func-name ()
               ,doc
               (interactive)
               (my/persp-open-project ,persp ,path ',mode))))
        projects)))

(my/setup-quick-projects

 ;; Configs (dotfiles
  (:name dotfiles
   :mode magit
   :persp "Dotfiles"
   :path "~/dotfiles")
  (:name emacs-config
   :mode magit
   :persp "Emacs-config"
   :path "~/.config/emacs")

  ;; Projects
  (:name herb
   :mode magit
   :persp "Herb"
   :path "~/_projects/clones/herbstluftwm")
  (:name vasiniyo
   :mode magit
   :persp "Vasiniyo"
   :path "~/_projects/clones/vasiniyo-chat-bot")
  (:name piechat
   :mode dired
   :persp "Piechat"
   :path "~/_projects/piechat")

  ;; notes / chats
  (:name lisp-notes
   :mode dired
   :persp "Lisp-notes"
   :path "~/notes/learn/lisp")
  (:name notes
   :mode dired
   :persp "notes"
   :path "~/notes")
  )


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
     "C-M-l" 'tab-line-switch-to-next-tab
     "C-M-h" 'tab-line-switch-to-prev-tab
     "M-i" 'evil-switch-to-windows-last-buffer
     "C-S-H" 'evil-window-left
     "C-S-J" 'evil-window-bottom
     "C-S-K" 'evil-window-up
     "C-S-L" 'evil-window-right
     )
     (general-define-key
     :states  '(normal motion)
     :keymaps 'override
     "s" #'avy-goto-char-2
     )

     ;; Consult-commands
     (general-create-definer my-jump-leader
       :states '(normal visual)
       :keymaps 'override
       :prefix "C-f")
     (my-jump-leader
       ;; projects
       "C-e"     '(persp-switch-last :which-key "persp last")
       "O"       '(projectile-switch-project :which-key "project [o]pen")
       "C-w"     '(persp-switch      :which-key "persp switch")
       ;; Specific projectiles:
       "o"       '(:ignore t :which-key "[o]pen file")
       "o j"     '(:ignore t :which-key "open pro[j]ect")
       "o d"     '(:ignore t :which-key "open [d]otfile")
       "o o"     '(:ignore t :which-key "open n[o]tes/[o]rg")

       "o d d"     '(my/open-dotfiles       :which-key "[D]otfiles (persp+magit)")
       "o d e"     '(my/open-emacs-config   :which-key "[E]macs (persp+magit)")
       "o o l"     '(my/open-lisp-notes     :which-key "[L]isp-notes (persp+dired)")
       "o o o"     '(my/open-notes          :which-key "[N]otes (persp+dired)")
       "o j c"     '(my/open-piechat        :which-key "[P]iechat (persp+dired)")
       "o j h"     '(my/open-herb           :which-key "[H]erb (persp+magit)")
       "o j v"     '(my/open-vasiniyo       :which-key "[V]asiniyo (persp+magit)")
       
       ;; consult multi-files
       "C-f"     '(consult-project-buffer :which-key "buffers")
       "F"       '(projectile-find-file   :which-key "[f]ind file")
       "G"       '(consult-ripgrep        :which-key "r[g]")
       "S"       '(consult-lsp-symbols    :which-key "lsp [S]ymbols")

       "I"     '(consult-info             :which-key "[i]nfo")
       "D"     '(devdocs-lookup           :which-key "[d]ocs lookup")
       ;; TODO: pre-complited infos
       ;; TODO: docs

       ;; consult in-buffer
       "i"   '(consult-imenu               :which-key "[i]menu")
       "l"   '(consult-outline             :which-key "out[l]ine")
       "j"   '(evil-collection-consult-jump-list :which-key "evil [j]ump list")
       "m"   '(evil-collection-consult-mark      :which-key "evil [m]arks")
       "C-s" '(consult-lsp-file-symbols          :which-key "lsp file [s]yms")
     )

     ;; SPC prefix
     (general-create-definer my-leader
       :states '(normal visual)
       :keymaps 'override
       :prefix "SPC"
       :non-normal-prefix "M-SPC")

     (my-leader
       ;; perspectives
       "C-j"         '(:ignore t :which-key "[+] persp-switch")
       "C-j C-j"     '(persp-switch :which-key "[+] persp-switch")
       "C-j i"       '(tab-bar-switch-to-recent-tab   :which-key "recent tab")
       "C-j e"       '(projectile-previous-project-buffer   :which-key "recent buffer")
       "C-j C-s"     '(persp-save   :which-key "save perspective")
       "C-j C-e"     '(persp-switch-last :which-key "switch last")
       ;; TODO eyebrowse
       "C-j C-h"     '(:ignore t :which-key "projectile managment")
       "C-j C-h C-h" '(projectile-switch-project :which-key "projectle switch project")
       ;; Specific projectiles:
       "C-j C-h D"   '(my/open-dotfiles :which-key "[D]otfiles (persp+magit)")
       "C-j C-h H"   '(my/open-herb :which-key "[H]erb (persp+magit)")
       "C-j C-h V"   '(my/open-vasiniyo :which-key "[V]asiniyo (persp+magit)")
       
       ;; “SPC f” prefix for “search” or “files”
       "f"   '(:ignore t :which-key "[+] search / files")
       "f f" '(projectile-find-file :which-key "projectile-find-file")
       "f j" '(consult-project-buffer :which-key "consult-project-buffer")
       "f J" '(pop-to-buffer :which-key "pop-to-buffer")
       "f g" '(consult-ripgrep :which-key "consult-r[g]")
       "f G" '(projectile-ripgrep :which-key "projectile r[g]")
       "f s" '(consult-lsp-file-symbols :which-key "lsp symbols")
       "f S" '(consult-lsp-symbols :which-key "lsp symbols")
       "f e" '(projectile-dired :which-key "dired in project")
       "f i" '(next-buffer     :which-key "next buffer")
       "f o" '(previous-buffer :which-key "previous buffer")

       ;; Buffer
       "y"   '(avy-copy-line :which-key "avy-copy-line")

       ;; “SPC t” tab managment
       "t"   '(:ignore t :which-key "tabs managment")
       "t n"   '(tab-new-to :which-key "new tab")
       "t c"   '(tab-close :which-key "close tab")
       "t t"   '(other-tab-prefix :which-key "other-tab-prefix")

       ;; "SPC g" for git
       "g"   '(:ignore t :which-key "git managment")
       "g g"   '(magit :which-key "magit")

       ;; “SPC h” prefix for help or “completions”
       "h"   '(:ignore t :which-key "[+] help / completions")
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
       "h g" '(:ignore t :which-key "[+] ma[g]it")
       "h g d" '((lambda () (interactive) (my/exec-with-prefix "vdiff-magit- "))
              :which-key "v[d]iff")

       ;; “SPC j” for LSP multi-file navigation
       "j"   '(:ignore t :which-key "[+] lsp")
       "j h" '(lsp-clangd-find-other-file        :which-key "switch .[h]pp/.cpp")
       "j l" '(lsp-find-references               :which-key "find references")
       "j r" '(lsp-rename                        :which-key "smart [r]ename")
       "j s" '(lsp-signature                     :which-key "lsp [s]ignature")
       "j d" '(lsp-find-definition               :which-key "goto definition")
       "j i" '(lsp-find-implementation           :which-key "goto [i]mpl.")
       "j w" '(lsp-ui-doc-toggle                 :which-key "toggle doc")
       "j t" '(lsp-find-type-definition          :which-key "[t]ype definition")

       ;; “SPC b” for buffer operations
       "b"   '(:ignore t :which-key "buffer")
       "b e" '(eval-buffer :which-key "evaluate buffer")

       ;; “SPC S” for system ops
       "S"   '(:ignore t :which-key "system")
       "S f" '(list-faces-display :which-key "list faces")

       ;; interface
       "u e" '(treemacs-select-window :which-key "treemacs")
       "u f" '(flycheck-list-errors :which-key "flycheck-list-errors")

       )
     )))

;; =============================== EVIL NORMAL KEYMAPS ===============================

;; ============================== MODE SPECIFIC KEYMAPS ==============================
;; (require dash)
;; (defmacro define-evil-keys-for-maps (state maps &rest bindings)
;;   "Define BINDINGS for STATE in multiple MAPS.
;; MAPS is a list of map symbols.
;; BINDINGS are pairs of (key . command)."
;;   =(dolist (map-sym ',maps)
;;      (evil-define-key ',state (symbol-value map-sym)
;;        ,@(cl-loop for (key . cmd) in bindings
;;                   append (list =(kbd ,key) =#',cmd)))))

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
  (evil-define-key 'normal magit-status-mode-map
    (kbd "M-s M-s") #'magit-stage
    (kbd "C-x 1") #'magit-section-show-level-1-all
    (kbd "C-x 2") #'magit-section-show-level-2-all
    (kbd "C-x 3") #'magit-section-show-level-3-all
    (kbd "C-x 4") #'magit-section-show-level-4-all)
  (evil-define-key 'normal magit-log-mode-map
    (kbd "C-x 1") #'magit-section-show-level-1-all
    (kbd "C-x 2") #'magit-section-show-level-2-all
    (kbd "C-x 3") #'magit-section-show-level-3-all
    (kbd "C-x 4") #'magit-section-show-level-4-all)
  (evil-define-key 'normal magit-stash-mode-map
    (kbd "C-x 1") #'magit-section-show-level-1-all
    (kbd "C-x 2") #'magit-section-show-level-2-all
    (kbd "C-x 3") #'magit-section-show-level-3-all
    (kbd "C-x 4") #'magit-section-show-level-4-all)
  (evil-define-key 'normal magit-revision-mode-map
    (kbd "C-x 1") #'magit-section-show-level-1-all
    (kbd "C-x 2") #'magit-section-show-level-2-all
    (kbd "C-x 3") #'magit-section-show-level-3-all
    (kbd "C-x 4") #'magit-section-show-level-4-all)
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
    (kbd "C-f C-d") #'consult-flycheck
    (kbd "C-j") #'flycheck-next-error
    (kbd "C-k") #'flycheck-previous-error
    (kbd "M-j") #'compilation-next-error
    (kbd "M-k") #'compilation-previous-error
  ))

(with-eval-after-load 'flyspell
  (evil-define-key 'normal flyspell-mode-map
    (kbd "C-f C-d") #'consult-flyspell
    (kbd "M-j") #'flyspell-goto-next-error
    (kbd "M-k") #'flycheck-previous-error
  ))

(with-eval-after-load 'lsp-ui
  (evil-define-key 'normal lsp-ui-mode-map
    (kbd "C-c d d") #'lsp-ui-doc-glance
  ))

(with-eval-after-load 'flyspell
  (evil-define-key 'normal org-mode-map
    (kbd "C-f C-w") #'consult-flyspell))

(evil-define-key 'normal org-mode-map
  (kbd "C-f i") #'consult-org-heading)

(with-eval-after-load 'devdocs
  (evil-define-key 'normal devdocs-mode-map
  (kbd "n") #'devdocs-go-forward
  (kbd "p") #'devdocs-go-back
  ))


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
;; So it can use evil-functions
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

(provide 'general-setup)
;;; general-setup.el ends here
