(setq evil-want-keybinding nil)  ;; Avoid conflicts with evil-collection
(setq evil-want-C-u-scroll t)    ;; Enable `C-u` for scrolling
(setq evil-shift-width 4)        ;; Set default indentation width

(use-package evil
  :ensure t
  :after evil
  :init
  ;; evil-collection intregration
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  
  (setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
  (setq-default tab-width 4)          ;; Set tab width
  (setq-default evil-shift-width 4)   ;; Match Doom’s default indentation width

  ;; logival lines navigation
  (setq evil-respect-visual-line-mode nil)
  (setq evil-cross-lines t)

  :config
  ;; Make evil motions work with visual lines
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-motion-state-map "0" 'evil-beginning-of-visual-line)
  (define-key evil-motion-state-map "$" 'evil-end-of-visual-line)
  (define-key evil-motion-state-map "^" 'evil-first-non-blank-of-visual-line)
  (define-key evil-motion-state-map "_" 'evil-first-non-blank-of-visual-line)
  
  ;; For visual state as well
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

  (evil-mode 1)
 )


(use-package evil-escape
  :after evil
  :config
  (setq-default evil-escape-key-sequence "fj")
  (setq-default evil-escape-delay 0.3)
  (evil-escape-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional shift todo heading)))

(with-eval-after-load 'org
  (require 'org-tempo))  ;; Enable tempo-based expansion

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-snipe
  :after evil
  :ensure t
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)) ;; enables s/S in ALL modes, not just visual

(use-package evil-nerd-commenter
  :after evil
  :bind
  (("M-/" . evilnc-comment-or-uncomment-lines))
  :config
  (evilnc-default-hotkeys))


(use-package evil-terminal-cursor-changer  
  :init (evil-terminal-cursor-changer-activate))  


;; FOLD
;; (use-package origami
;;   :ensure t
;;   :hook ((prog-mode . origami-mode))
;;   :bind (:map evil-normal-state-map
;;               ("za" . origami-toggle-node)
;;               ("zc" . origami-close-node)
;;               ("zo" . origami-open-node)
;;               ("zr" . origami-open-all-nodes)
;;               ("zm" . origami-close-all-nodes)))
