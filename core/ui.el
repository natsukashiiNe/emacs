;; --- Force the tab bar to always show ---
(setq tab-bar-show 'always)
(tab-bar-mode 1)

;; Left: {persp: proj} | tabs
(defun exu/tab-bar-left ()
  (let* ((persp (if (and (boundp 'persp-mode) persp-mode)
                    (if (fboundp 'persp-name)
                        (persp-name (get-current-persp))
                      "NoPersp")
                  "NoPersp"))
         (proj (if (and (boundp 'projectile-mode) projectile-mode)
                   (if (fboundp 'projectile-project-name)
                       (projectile-project-name)
                     "NoProj")
                 "NoProj")))
    (format " [%s|%s] " persp proj)))

(defun exu/diagnostics-string ()
  (if (and (boundp 'flycheck-mode) flycheck-mode
           (boundp 'flycheck-current-errors) flycheck-current-errors)
      (let* ((counts (flycheck-count-errors flycheck-current-errors))
             (err (or (cdr (assq 'error counts)) 0))
             (warn (or (cdr (assq 'warning counts)) 0))
             (info (or (cdr (assq 'info counts)) 0)))
        (format " [E:%d W:%d I:%d] " err warn info))
    ""))

(defun exu/git-branch-string ()
  (if (and buffer-file-name vc-mode)
      (let ((branch (if (string-match "[^:]+: \\(.*\\)" vc-mode)
                        (match-string 1 vc-mode)
                      vc-mode)))
        (format " [ %s] " branch))
    ""))

;; Right: git | diagnostic
(defun exu/tab-bar-right ()
  (concat (exu/diagnostics-string) (exu/git-branch-string)))

(setq tab-bar-format
      `((:eval (exu/tab-bar-left))
        tab-bar-tabs
        (:eval (let ((right (exu/tab-bar-right)))
                 (propertize " " 'display `(space :align-to (- right ,(+ 2 (length right)))))))
        (:eval (exu/tab-bar-right))))
