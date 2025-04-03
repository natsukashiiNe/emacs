(load (expand-file-name "load_packages.el" (file-name-directory (or load-file-name buffer-file-name))))

;; DS to store info about my sessions
(defvar elastic--sessions (make-hash-table :test 'equal)
  "Stores per-perspective elastic session data.
  (:tabs (list of tab names)
   :child-frame <frame or nil>
   :terminals <hash-table of name => (buffer . mode)>)")

;; def parameter is always current persp session. session that has binded tabs and child frames is named "elastic"
;; add tab to elastic (creates a new tab "globally" and adds it to the named OR current elastic)
;; add child-frame to elastic (creates a new child frame and adds it to the named OR current elastic)
;; add terminals to elastic (creates a new teminal in the given mode and adds it to the named OR current elactic)
;; new_elastic (creates a new entry in the map with respective "starting" tab and binded child frame)

(defun elastic-session-exists-p (&optional name)
  "Return non-nil if an elastic session for the given NAME exists."
  (let ((pname (or name (persp-current-name))))
    (gethash pname elastic--sessions)))

(defun my-get-persp-data (&optional persp-name)
  (let* ((pname (or persp-name (persp-current-name)))
         (entry (gethash pname my--persp-data))) 
    (unless entry
      (setq entry (list :child-frame nil
                        :terminals (make-hash-table :test 'equal)
                        :tabs (list)))
      (puthash pname entry my--persp-data))
    entry))

