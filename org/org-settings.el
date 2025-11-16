(setq package--init-file-ensured t) ;; Workaround for package.el trying to autoload
(setq load-prefer-newer t) ;; Always prefer newer built-in files
(setq org-modules nil) ;; Don't autoload extra Org modules
;; (add-hook 'org-mode-hook #'hide-mode-line-mode) ;; disable status line in org mode TODO make it just different

(setq org-use-property-inheritance t)
(setq org-startup-indented t)        ;; Pretty indentation
(setq org-pretty-entities t)         ;; Display symbols (like LaTeX-style)
(setq org-ellipsis " 󰞖 ")             ;; Make collapsible sections look better
(setq org-hide-leading-stars t)      ;; Hide extra stars in headlines
(setq org-special-ctrl-a/e t)        ;; More predictable movement in lists
(setq org-use-speed-commands t)      ;; Speed commands (useful for large org files)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode) ;; Indent wrapped lines nicely
(setq org-export-preserve-breaks t) ;; new line = always breaks line

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

(setq org-latex-compiler "xelatex")

(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[12pt]{article}
                  \\usepackage{fontspec}
                  \\setmainfont{GoMono Nerd Font}
                  \\newfontfamily\\cyrillicfont{GoMono Nerd Font}
                  \\setmonofont{GoMono Nerd Font}
                  \\usepackage[russian,english]{babel}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))



;; LATEX
(setq org-latex-default-figure-position "H")

(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :config
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type "txt"))  ; Default to text output

;; Configure org-babel for PlantUML
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

;; Set default to ASCII art output (no file needed)
(setq org-babel-default-header-args:plantuml
      '((:results . "verbatim")
        (:cmdline . "-txt")))

;; BABEL
;; BABEL CONFIGURATION (Add this section)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (lisp . t)
   (emacs-lisp . t)
   (latex . t)
   (plantuml . t)))

(setq org-babel-lisp-eval-fn 'sly-eval)  ; Use SLY to evaluate
(setq org-confirm-babel-evaluate nil)

;; (setq org-confirm-babel-evaluate nil)  ; Or use the selective function above
(setq org-babel-python-command "python3")
(setq org-babel-default-header-args:python
      '((:results . "output")))

(defun my-org-babel-ansi-colorize ()
  "Apply ANSI color codes to the results of the current babel block."
  (let ((beg (save-excursion
               (goto-char (or (org-babel-where-is-src-block-result) (point-min)))
               (forward-line)
               (point)))
        (end (save-excursion
               (goto-char (or (org-babel-where-is-src-block-result) (point-min)))
               (org-babel-result-end))))
    (ansi-color-apply-on-region beg end)))

(add-hook 'org-babel-after-execute-hook #'my-org-babel-ansi-colorize)
