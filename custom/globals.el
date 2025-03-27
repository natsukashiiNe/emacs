;; TODO + add entry to some data file to later check for intersections
;; TODO + add remap entry for russian layout via:
;; - (define-key key-translation-map (kbd "C-г") (kbd "C-c"))
;; - (define-key input-decode-map [?\C-у] [?\C-h])

(defmacro keymap-set-with-desc (&rest args)
  "Bind a key in a keymap and add a which-key description.
Keyword arguments:
  :map     -- the keymap to use
  :key     -- the key sequence 
  :ru_key  -- russian key
  :command -- the command to bind (can be nil)
  :desc    -- the description (for which-key)"
  (let ((map     (plist-get args :map))
        (key     (plist-get args :key))
        (ru_key  (or (plist-get args :ru_key) (plist-get args :key)))
        (command (plist-get args :command))
        (desc    (plist-get args :desc)))
    `(progn
       (keymap-set ,map ,key ,command)
       (keymap-set ,map ,ru_key ,command)
       (which-key-add-key-based-replacements ,key ,desc))))



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
