;; TODO + add entry to some data file to later check for intersections
;; TODO + add remap entry for russian layout via:
;; - (define-key key-translation-map (kbd "C-г") (kbd "C-c"))
;; - (define-key input-decode-map [?\C-у] [?\C-h])

(defmacro keymap-set-with-desc (&rest args)
  "Bind a key in multiple keymaps and add a which-key description.

Keyword arguments:
  :maps    -- a list of keymaps (symbols or actual keymap objects)
  :key     -- the primary key sequence
  :ru_key  -- an alternate “Russian” key (defaults to :key if omitted)
  :command -- the command to bind (can be nil for prefix)
  :desc    -- the which-key label"
  (let ((maps    (plist-get args :maps))
        (key     (plist-get args :key))
        (ru-key  (or (plist-get args :ru_key) (plist-get args :key)))
        (command (plist-get args :command))
        (desc    (plist-get args :desc)))
    ;; Generate a dolist for run-time binding
    `(dolist (m ,maps)
       ;; If M is a symbol, look up its value (the actual keymap).
       (let ((km (if (symbolp m)
                     (symbol-value m)
                   m)))
         (unless (keymapp km)
           (error "Not a keymap: %S" km))
         ;; Bind both the normal key and the `ru-key`.
         (keymap-set km ,key ,command)
         (keymap-set km ,ru-key ,command)
         ;; Assign a which-key description.
         (which-key-add-key-based-replacements ,key ,desc)))))




(defun my/toggle-messages-window ()
  "Toggle the *Messages* buffer in a selectable bottom window."
  (interactive)
  (let ((buffer (get-buffer "*Messages*")))
    (if (and buffer (get-buffer-window buffer))
        (delete-window (get-buffer-window buffer))  ;; Close if open
      (display-buffer buffer
                      '((display-buffer-in-side-window)
                        (side . bottom)
                        (window-height . 0.3)
                        (window-parameters . ((no-other-window . nil) ;; Allow selection
                                              (no-delete-other-windows . nil))))))))

(global-set-key (kbd "C-c m") #'my/toggle-messages-window)

(defun show-buffer-and-file ()
  "Show current buffer name and file path."
  (interactive)
  (if buffer-file-name
      (message "Buffer: %s\nFile: %s" (buffer-name) buffer-file-name)
    (message "Buffer: %s (not visiting a file)" (buffer-name))))
