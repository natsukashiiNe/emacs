;;; init.el --- Startup file -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs init file

;;; Code:

;; Enable native compilation if available
(when (and (fboundp 'native-comp-available-p)
            (native-comp-available-p))
    (setq native-comp-deferred-compilation t
        native-comp-async-report-warnings-errors nil))

;; ------------------------------------------------------------------------------
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

;; ------------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path (expand-file-name "themes" my-config-dir))

(add-to-list 'load-path (expand-file-name "apps"   my-config-dir))
(add-to-list 'load-path (expand-file-name "core"   my-config-dir))
(add-to-list 'load-path (expand-file-name "custom" my-config-dir))
(add-to-list 'load-path (expand-file-name "lsp"    my-config-dir))
(add-to-list 'load-path (expand-file-name "org"    my-config-dir))
(add-to-list 'load-path (expand-file-name "themes" my-config-dir))
;; ----------------------------------------------------------------------

(load-config-file "core/straight.el")   ;; Package Manager (straight.el)
(load-config-file "core/env_settings.el")

(load-config-file "custom/globals.el")
(load-config-file "core/settings.el")
(load-config-file "themes/parameters.el")
(load-config-file "core/evil.el")

(load-config-file "core/completion/orderless.el")  ;; Better Matching
(load-config-file "core/completion/avy.el")        ;; "jump anywhere"
(load-config-file "core/completion/vertico.el")    ;; Minibuffer Navigation
(load-config-file "core/completion/marginalia.el") ;; Metadata Display
(load-config-file "core/completion/consult.el")    ;; Search & Navigation
(load-config-file "core/completion/corfu.el")      ;; Auto-Completion
(load-config-file "core/completion/embark.el")     ;; Actions & Selection

;; TMUX
(load-config-file "session-manager/main.el")       ;; Perps + Projectile (remaking this)
;; (load-config-file "session-manager/daemon-persistence.el")
;; (load-config-file "core/frame-setup.el")

;; Enable daemon persistence
;; (when (daemonp)
;;   (daemon-persistence-mode 1))
;; (load-config-file "core/centaur-tabs.el")
(load-config-file "core/vterm.el")
(load-config-file "core/posframe-settings.el")
(load-config-file "core/tab-bar.el")

(load-config-file "org/org-settings.el")  ;; Org-mode customizations
(load-config-file "org/org-plugins.el")   ;; Org-mode customizations

;; TODO lsp configuration
(load-config-file "lsp/lsp-modes.el")

(load-config-file "lsp/treesitter.el")
;; Load LSP configs first
(require 'lsp-config)
(require 'lsp-servers)
;; Then load Lisp dev setup (replaces sbcl-setup.el)
(load-config-file "lsp/lisp-dev-setup.el")

;; (load-config-file "lsp/test_lsp.el")
(load-config-file "lsp/lsp-ui.el")
;; TODO move to require
(load-config-file "lsp/diagnostics.el")
(load-config-file "core/ftree.el")         ;; why is this so bad

;; CUSTOM
(load-config-file "core/general-setup.el")
(load-config-file "custom/elastic.el")  ;; Floating Frames Controls (TODO)

;; GUI: Apply after to the frame
(load-config-file "core/which-key.el")
(load-config-file "core/ui.el")

;; APPS
(load-config-file "apps/decrypt-keys.el")

(load-config-file "apps/telega.el")
(load-config-file "apps/magit.el")

;; 


;; Conditionally loaded files:
(defun load-gptel-config ()
  (interactive)
  (load-config-file "apps/gptel-setup.el"))


(message "🎉 Emacs startup complete!")
;; ----------------------------------------------------------------------
;; END OF CONFIG (DAEMON)
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
;; GUI 
;; ----------------------------------------------------------------------
;; (defun my/setup-gui-frame (frame)
;;   "Apply GUI settings after a new FRAME is created."
;;   (with-selected-frame frame
;;     (message "⚡ Setting up GUI frame!")
;;     (my/setup-ui)
;;     (my/setup-theme-parameters)


;;     (mapc #'disable-theme custom-enabled-themes)
;;     (load-theme 'test t)
;;     (blink-cursor-mode 0)
;;     (set-face-background 'child-frame-border "#FF8020"))
;;   (message "⚡ Setting up GUI frame [FINISHED]!")
;;   )

;; (add-hook 'after-make-frame-functions #'my/setup-gui-frame)
