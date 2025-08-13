(require 'pyvenv)

(defun my/eaf-ensure-venv ()
  "Ensure that the EAF virtual environment exists and has all required packages installed."
  (let* ((eaf-venv-dir (expand-file-name "eaf-venv" (file-name-directory (or load-file-name buffer-file-name))))
         (requirements-file (expand-file-name "python_requirements.txt" (file-name-directory eaf-venv-dir)))
         (installed-flag (expand-file-name "installed.flag" eaf-venv-dir)))
    (unless (file-exists-p eaf-venv-dir)
      (message "* Creating Python venv for EAF at: %s" eaf-venv-dir)
      (call-process "python3" nil nil nil "-m" "venv" eaf-venv-dir))
    ;; If user never installed the dependencies, do it now (only once).
    (unless (file-exists-p installed-flag)
      (message "* Installing EAF Python deps from %s..." requirements-file)
      (let ((pip (expand-file-name "bin/pip" eaf-venv-dir)))
        (call-process pip nil nil nil "install" "--upgrade" "pip")
        (call-process pip nil nil nil "install" "-r" requirements-file))
      (with-temp-file installed-flag
        (insert "EAF Python dependencies installed.")))))

;; Now define our `my/eaf-init` to set up everything
(defun my/eaf-init ()
  "Initialize EAF in a dedicated Python environment, then load modules."
  (my/eaf-ensure-venv)
  (let ((eaf-venv-dir (expand-file-name "eaf-venv" (file-name-directory (or load-file-name buffer-file-name)))))
    ;; Activate the environment in Emacs
    (pyvenv-activate eaf-venv-dir)
    ;; Now load EAF via straight
    (straight-use-package
     '(eaf
       :type git
       :host github
       :repo "emacs-eaf/emacs-application-framework"
       :files ("*")
       :local-repo "emacs-application-framework"))
    ;; Then configure the EAF package
    (use-package eaf
      :init
      (require 'eaf)  ;; Core
      :config
      ;; Load whichever EAF modules you want:
      (require 'eaf-browser)
      (require 'eaf-pdf-viewer)
      (require 'eaf-file-browser)
      (require 'eaf-image-viewer)
      (require 'eaf-markdown-previewer)
      (require 'eaf-org-previewer)
      (require 'eaf-terminal)
      (require 'eaf-file-sender)
      ;; ... add or remove modules as desired

      ;; Basic EAF customizations
      (setq eaf-browser-remember-history t
            eaf-browser-enable-adblocker t)

      ;; Example: Keybinding to open the EAF browser:
      (global-set-key (kbd "C-c e b")
                      (lambda () (interactive)
                        (eaf-open-browser "https://www.example.com")))

      (message "* EAF loaded successfully!"))))

;; Finally, run our initialization:
(my/eaf-init)

(provide 'eaf-setup)
;;; eaf-setup.el ends here
