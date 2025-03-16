;; TODO + add entry to some data file to later check for intersections
(defmacro keymap-set-with-desc (&rest args)
  "Bind a key in a keymap and add a which-key description.
Keyword arguments:
  :map     -- the keymap to use
  :key     -- the key sequence 
  :command -- the command to bind (can be nil)
  :desc    -- the description (for which-key)
"
  (let ((map     (plist-get args :map))
        (key     (plist-get args :key))
        (command (plist-get args :command))
        (desc    (plist-get args :desc)))
    `(progn
       (keymap-set ,map ,key ,command)
       (which-key-add-key-based-replacements ,key ,desc))))
