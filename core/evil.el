(setq evil-want-keybinding nil)  ;; Avoid conflicts with evil-collection
(setq evil-want-C-u-scroll t)    ;; Enable `C-u` for scrolling
(setq evil-shift-width 4)        ;; Set default indentation width

(use-package evil
  :ensure t
  :init
  ;; evil-collection intregration
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  
  (setq-default indent-tabs-mode nil) ;; Use spaces instead of tabs
  (setq-default tab-width 4)          ;; Set tab width
  (setq-default evil-shift-width 4)   ;; Match Doom’s default indentation width

  :config
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

(use-package evil-snipe)
