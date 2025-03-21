;; dictionary
(use-package flyspell
  :hook (org-mode . flyspell-mode)  ;; Enable spell checking in Org mode
  :config
  (setq ispell-program-name "aspell") ;; Use Aspell
  (setq ispell-dictionary "ru") ;; Set Russian as the default dictionary
  (setq ispell-extra-args '("--sug-mode=ultra")) ;; Faster suggestions
  (setq flyspell-issue-message-flag nil)) ;; Prevent annoying messages
(setq ispell-program-name "aspell") ;; Ensure Aspell is used

;; integration with vertigo
(use-package flyspell-correct
:after flyspell
:bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;; Napking integration
(use-package ob-napkin
  :ensure t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(napkin . t))) ;; Adjust path if using pipx


