;;; flycheck-setup.el --- Configuration for flycheck -*- lexical-binding: t; -*-
    
;;; Commentary:
;; Flycheck pacakges and its supportive packages

;;; Code:

(use-package flycheck
  :commands (flycheck-mode)
  :functions (flycheck-define-error-level)  ;; tells compiler it exists
  :custom
  (flycheck-idle-change-delay 0.1)
  (flycheck-display-errors-delay 0.1)
  :init
  (defvar lsp-managed-mode nil)
  (defun my/flycheck-toggle-for-lsp-managed ()
    "Enable Flycheck when LSP manages this prog buffer; disable otherwise."
    (if (and lsp-managed-mode (derived-mode-p 'prog-mode))
        (flycheck-mode 1)
      (flycheck-mode -1)))
  :hook (lsp-managed-mode . my/flycheck-toggle-for-lsp-managed)
  :config
  ;; (was in with-eval-after-load) — now we are after flycheck is loaded.
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [256 256 256 256 256 256 256 256] 16 16 'center)
  (define-fringe-bitmap 'test-fringe
    [#b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111
     #b1111111111111111]
    16 16 '(center repeated))
  (flycheck-define-error-level 'error
    :severity 100 :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'test-fringe :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 50 :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'test-fringe :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 10 :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'test-fringe :fringe-face 'flycheck-fringe-info))


;; Quick-peek (tooltip-like overlays we’ll reuse for inline messages)
(use-package quick-peek
  :custom
  (quick-peek-add-spacer nil)
  :config
  (set-face-attribute 'quick-peek-background-face nil
                      :background nil :inherit 'default :extend t)
   :custom
   (quick-peek-add-spacer nil))


;; flycheck-inline: define our helper *before* :custom; declare quick-peek fns.
(use-package flycheck-inline
  :after (flycheck quick-peek)
  :hook (flycheck-mode . flycheck-inline-mode)

  :preface
  (declare-function quick-peek-overlay-ensure-at "quick-peek" (pos))
  (declare-function quick-peek-overlay-contents  "quick-peek" (ov))
  (declare-function quick-peek-update            "quick-peek" (ov))
  (declare-function quick-peek-hide              "quick-peek" ())

  (defun my/flycheck-inline-quick-peek (msg pos _err)
    (let* ((ov (quick-peek-overlay-ensure-at pos))
           (contents (quick-peek-overlay-contents ov)))
      (setf (quick-peek-overlay-contents ov)
            (concat contents (when contents "\n") msg))
      (quick-peek-update ov)))

  :custom
  (flycheck-inline-display-function #'my/flycheck-inline-quick-peek)
  (flycheck-inline-clear-function   #'quick-peek-hide))

;; (use-package flycheck-popup-tip
;;   :after flycheck
;;   :hook (flycheck-mode . flycheck-popup-tip-mode))

                                        ; Optional: flyover (virtual line guides for errors)
(use-package flyover
  :after flycheck
  :custom
  (flyover-virtual-line-type 'line-no-arrow)
  (flyover-show-virtual-line t)
  :custom-face
  (flyover-error   ((t (:weight normal))))
  (flyover-warning ((t (:weight normal))))
  (flyover-info    ((t (:weight normal))))
  ;; Enable manually if you like:
  ;; :hook (flycheck-mode . flyover-mode)
  )


(provide 'flycheck-setup)
;;; flycheck-setup.el ends here
