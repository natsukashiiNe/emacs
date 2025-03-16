(setq package--init-file-ensured t) ;; Workaround for package.el trying to autoload
(setq load-prefer-newer t) ;; Always prefer newer built-in files
(setq org-modules nil) ;; Don't autoload extra Org modules
;; (add-hook 'org-mode-hook #'hide-mode-line-mode) ;; disable status line in org mode TODO make it just different

(setq org-use-property-inheritance t)
(setq org-startup-indented t)        ;; Pretty indentation
(setq org-pretty-entities t)         ;; Display symbols (like LaTeX-style)
(setq org-ellipsis " ▾")             ;; Make collapsible sections look better
(setq org-hide-leading-stars t)      ;; Hide extra stars in headlines
(setq org-special-ctrl-a/e t)        ;; More predictable movement in lists
(setq org-use-speed-commands t)      ;; Speed commands (useful for large org files)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode) ;; Indent wrapped lines nicely

(setq org-startup-with-inline-images t) ;; Show images when opening an Org file
(setq org-image-actual-width nil)       ;; Scale images to their actual width

;; Enable persistent todo states tracking
(setq org-log-done 'time)
(setq org-log-into-drawer t)  ;; Store logs in a drawer for a cleaner view
;; bullets instead of asteriks
;; (add-hook 'org-mode-hook 'org-indent-mode)

(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))  ;; Largest
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-level-6 ((t (:inherit outline-6 :height 1.0))))
 '(org-level-7 ((t (:inherit outline-7 :height 1.0))))
 '(org-level-8 ((t (:inherit outline-8 :height 1.0)))))

(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '(" " "◉" "●" "󰧂" "󰘍")
        org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?•) (?- . ?➤))))
