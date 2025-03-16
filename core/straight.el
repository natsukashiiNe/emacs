(setq straight-base-dir "~/.emacs.d/")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir)) ;; ⬅️ Uses `straight-base-dir`
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Ensure `use-package` is installed via Straight
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Optimize package modification checks
(setq straight-check-for-modifications '(find-when-checking))

;; Ensure built-in Org is recognized correctly
(require 'org)
(setq straight-built-in-pseudo-packages '(org))

;; Set up MELPA, GNU, and NonGNU package repositories
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu". "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
