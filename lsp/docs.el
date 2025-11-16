;;; docs.el --- Documentation -*- lexical-binding: t; -*-

;;; Commentary:
;; Packages and setup for docs for various langs and tools

;;; Code:

(use-package devdocs
  :ensure t
  :defer t
  :bind ("C-c d" . devdocs-lookup))  ;; quick lookup

(use-package dash-docs
  :ensure t
  :defer t
  :config
  (setq dash-docs-docsets-path "~/.emacs.d/docsets")
  (setq dash-docs-enable-debugging nil)
  (setq dash-docs-browser-func #'eww-browse-url)
  
  ;; Download C++ docsets
  (defun my/install-cpp-docsets ()
    "Install C++ related docsets."
    (interactive)
    (dash-docs-install-docset "C++")
    (dash-docs-install-docset "Boost")
    (dash-docs-install-docset "Qt_6")
    (dash-docs-install-docset "CMake"))
  
  ;; Mode-specific docsets
  (add-hook 'c++ts-mode-hook
            (lambda () (setq-local dash-docs-docsets
                                   '("C++" "Boost" "Qt_6")))))

(use-package counsel-dash
  :ensure t
  :defer t
  ;;:bind (("C-c d h" . counsel-dash))
  :config
  (setq counsel-dash-browser-func 'eww))

(use-package helm-dash
  :ensure t
  :defer t
  :bind ("C-c z" . helm-dash))

(use-package org-web-tools
  :ensure t)

(defun my/dash-docs-insert-as-org ()
  "Convert the current Dash/Zeal doc page in EWW to real Org markup."
  (interactive)
  (unless (eq major-mode 'eww-mode)
    (user-error "This must be run from the EWW buffer that dash-docs opened"))
  (org-web-tools-read-url-as-org (plist-get eww-data :url)))


;; (provide docs)
;;; docs.el ends here
