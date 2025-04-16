;; encryption:
(require 'yaml)
(require 'epa-file)
(epa-file-enable)

(setq auth-sources '("~/.authinfo.gpg")) ;; Magit
(setq epa-pinentry-mode 'loopback) ;; allows GPG passphrase prompts in Emacs

;; API KEYS RETRIEVING:
(defun my/openai-key-names ()
  "Get list of available OpenAI key names from the GPG-encrypted secrets file."
  (split-string
   (with-temp-buffer
     (let ((exit-code
            (call-process "python3" nil t nil
                          (expand-file-name "~/dotfiles/_scripts/choose_api_key.py")
                          "--mode" "list")))
       (unless (eq exit-code 0)
         (error "Failed to get OpenAI key list"))
       (buffer-string)))
   "\n" t))

(defun my/get-openai-key-value (key-name)
  "Retrieve the actual OpenAI key value from the GPG-encrypted YAML file."
  (string-trim
   (with-temp-buffer
     (let ((exit-code
            (call-process "python3" nil t nil
                          (expand-file-name "~/dotfiles/_scripts/choose_api_key.py")
                          "--mode" "get" "--key" key-name)))
       (unless (eq exit-code 0)
         (error "Failed to retrieve key for '%s'" key-name))
       (buffer-string)))))

(defun my/select-and-set-openai-key ()
  "Prompt using completing-read to choose a key, then retrieve and return its value."
  (interactive)
  (let* ((keys (my/openai-key-names))
         (selected (completing-read "Choose OpenAI key: " keys nil t)))
    (my/get-openai-key-value selected)))
