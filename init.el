;; ----------------------------------------------------------------------
(setq user-emacs-directory "~/.emacs.d/")
(defvar my-config-dir "~/.config/emacs/"
  "Directory that contains all the .el config files.")

(defun load-config-file (file)
  "Load an Emacs Lisp FILE from `user-emacs-directory` and report errors with file and line number."
  (let ((path (expand-file-name file my-config-dir)))
    (if (file-exists-p path)
        (condition-case err
            (progn
              (message "🔄 Loading: %s" file)
              (load-file path)
              (message "✅ Successfully loaded: %s" file))
          (error
           (message "❌ ERROR in %s: %s" file (error-message-string err))
           (with-current-buffer "*Messages*"
             (goto-char (point-max))
             (re-search-backward (format "Error in file %s" file) nil t)
             (message "💡 See error details above for file: %s" file))))
      (message "⚠️ Warning: Config file %s not found" file))))

;; ----------------------------------------------------------------------
(add-to-list 'custom-theme-load-path (expand-file-name "themes" my-config-dir))

(add-to-list 'load-path (expand-file-name "apps" my-config-dir))
(add-to-list 'load-path (expand-file-name "core" my-config-dir))
(add-to-list 'load-path (expand-file-name "custom" my-config-dir))
(add-to-list 'load-path (expand-file-name "lsp" my-config-dir))
(add-to-list 'load-path (expand-file-name "org" my-config-dir))
(add-to-list 'load-path (expand-file-name "themes" my-config-dir))
;; ----------------------------------------------------------------------


(load-config-file "core/straight.el")   ;; Package Manager (straight.el)
(load-config-file "core/env_settings.el")


(load-config-file "custom/globals.el")
(load-config-file "core/settings.el")
(load-config-file "core/evil.el")

(load-config-file "core/completion/vertico.el")    ;; Minibuffer Navigation
(load-config-file "core/completion/marginalia.el") ;; Metadata Display
(load-config-file "core/completion/consult.el")    ;; Search & Navigation
(load-config-file "core/completion/corfu.el")      ;; Auto-Completion
(load-config-file "core/completion/embark.el")     ;; Actions & Selection
(load-config-file "core/completion/orderless.el")  ;; Better Matching

;; TMUX
(load-config-file "session-manager/main.el")       ;; Perps + Projectile (remaking this)
;; (load-config-file "core/centaur-tabs.el")
(load-config-file "core/vterm.el")
(load-config-file "core/posframe.el")

(load-config-file "org/org-settings.el")  ;; Org-mode customizations
(load-config-file "org/org-plugins.el")   ;; Org-mode customizations

;; TODO lsp configuration 
(load-config-file "lsp/lsp-modes.el")

(load-config-file "lsp/lsp-config.el")
(load-config-file "lsp/tree-sitter.el")
(load-config-file "lsp/lsp-servers.el")
(load-config-file "lsp/lsp-ui.el")


;; (load-config-file "lsp/treesitter.el")
(load-config-file "core/ftree.el")         ;; why is this so bad

;; APPS
(load-config-file "apps/decrypt-keys.el")

(load-config-file "apps/telega.el")
(load-config-file "apps/magit.el")
(load-config-file "apps/gptel-setup.el")

;; CUSTOM
(load-config-file "custom/elastic.el")  ;; Floating Frames Controls (TODO)

;; (load-config-file "core/keymaps.el")  
(load-config-file "core/general-setup.el")  
(load-config-file "core/which-key.el")
(load-config-file "core/ui.el")
(load-config-file "themes/parameters.el")
(load-config-file "themes/modeline-options.el") ;; TODO

;; (load-config-file "themes/eXu.el") ;; TODO

;; staff that for some reason get rewritten after eval of settings file
(blink-cursor-mode 0)  ;; disable cursor blinking
;; "#FF8700"
;; "#FF8020"
(set-face-background 'child-frame-border "#FF8020")

(message "🎉 Emacs startup complete!")

;; ----------------------------------------------------------------------
;; END OF CONFIG
;; ----------------------------------------------------------------------

(mapc #'disable-theme custom-enabled-themes)
(load-theme 'test t)
;; (load-theme 'leuven t)


;; ----------------------------------------------------------------------
;; TERMINAL OVERRIDE
;; ----------------------------------------------------------------------
