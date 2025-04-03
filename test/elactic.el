;;; elastic.el --- Per-persp floating vterm setup with toggles  -*- lexical-binding: t; -*-

(require 'persp-mode)
(require 'vterm)
(require 'tab-bar)

;;; Data structure

;; This hash table will store the per-persp data:
;;   KEY: persp name (string)
;;   VALUE: a plist of:
;;      :child-frame -> the child frame object (or nil)
;;      :terminals   -> alist of (term-name . (buffer . mode))
;;
;; 'mode' is either 'child or 'main
;;
(defvar elastic--persp-terminals (make-hash-table :test 'equal)
  "Hash table storing child-frame and terminal info per persp.")

;;; Utilities

(defun elastic--get-persp-name ()
  "Return the name (string) of the current persp."
  (persp-current-name))

(defun elastic--get-persp-data (&optional persp-name)
  "Return the plist of the current (or given) persp from `elastic--persp-terminals`.
Initialize if not present."
  (let* ((pname (or persp-name (elastic--get-persp-name)))
         (data (gethash pname elastic--persp-terminals)))
    (unless data
      (setq data (list :child-frame nil :terminals nil))
      (puthash pname data elastic--persp-terminals))
    data))

(defun elastic--set-persp-data (key value &optional persp-name)
  "Set KEY in the plist of current (or given) persp to VALUE."
  (let* ((pname (or persp-name (elastic--get-persp-name)))
         (data (elastic--get-persp-data pname)))
    (plist-put data key value)
    (puthash pname data elastic--persp-terminals)
    value))

(defun elastic--get-child-frame (&optional persp-name)
  "Get the child frame for current (or given) persp, or nil."
  (plist-get (elastic--get-persp-data persp-name) :child-frame))

(defun elastic--set-child-frame (frame &optional persp-name)
  "Set the child frame for current (or given) persp."
  (elastic--set-persp-data :child-frame frame persp-name))

(defun elastic--get-terminals (&optional persp-name)
  "Return the alist of terminals for the current (or given) persp.
Each element is (term-name . (buffer . mode))."
  (plist-get (elastic--get-persp-data persp-name) :terminals))

(defun elastic--set-terminals (terms &optional persp-name)
  "Set the alist of terminals for the current (or given) persp to TERMS."
  (elastic--set-persp-data :terminals terms persp-name))

(defun elastic--get-terminal (term-name &optional persp-name)
  "Get the (buffer . mode) for TERM-NAME in current persp, or nil if not present."
  (cdr (assoc term-name (elastic--get-terminals persp-name))))

(defun elastic--add-terminal (term-name buffer mode &optional persp-name)
  "Add or update a terminal named TERM-NAME with BUFFER and MODE for the given persp."
  (let* ((terms (elastic--get-terminals persp-name))
         (entry (assoc term-name terms)))
    (if entry
        (setcdr entry (cons buffer mode)) ;; update existing
      (push (cons term-name (cons buffer mode)) terms))
    (elastic--set-terminals terms persp-name)))

;;; Child Frame Creation

(defvar elastic--floating-frame-parent (selected-frame)
  "Which frame to treat as the parent for any child frames.
You could dynamically set this whenever you switch persps, or rely on your main frame.")

(defun elastic--create-or-get-child-frame ()
  "Return a child frame for the current persp, creating it if needed.
Turns on `tab-bar-mode` in that child frame so each terminal is in its own tab."
  (let* ((cf (elastic--get-child-frame)))
    (unless (and cf (frame-live-p cf))
      ;; Create a new child frame
      (setq cf
            (make-frame
             `((parent-frame . ,elastic--floating-frame-parent)
               (title . "elastic-floating")
               (width . 80)
               (height . 24)
               (left . 100)
               (top . 50)
               (undecorated . nil)
               (visibility . nil)
               (no-accept-focus . nil)
               (child-frame-border-width . 4)
               ;; Possibly more params, see your snippet...
               )))
      ;; Turn on tab-bar-mode in the child frame, so we can do multiple vterms
      (with-selected-frame cf
        (tab-bar-mode 1)
        ;; you might want a custom tab-bar format, etc.
        )
      (elastic--set-child-frame cf))
    cf))

(defun elastic--child-frame-visible-p (cf)
  "Return non-nil if CF is live and visible."
  (and cf (frame-live-p cf) (frame-visible-p cf)))

;;; Terminal Creation

(defun elastic--create-vterm-buffer (term-name)
  "Create a new vterm buffer named after TERM-NAME, returning that buffer.
Does NOT set up tab naming or anything, just creates the buffer."
  (let ((bufname (format "*elastic-vterm:%s*" term-name)))
    ;; create vterm buffer
    (with-current-buffer (vterm (generate-new-buffer bufname))
      (setq-local mode-line-format nil)
      (current-buffer))))

(defun elastic--show-child-frame-terminal (term-name)
  "Show or create a vterm named TERM-NAME inside the child frame tab.
Switches to that tab in the child frame."
  (let* ((cf (elastic--create-or-get-child-frame))
         (term-pair (elastic--get-terminal term-name)) ;; => (buffer . mode)
         (buf (car term-pair))
         (mode (cdr term-pair)))
    (unless (and buf (buffer-live-p buf))
      ;; create new vterm buffer
      (setq buf (elastic--create-vterm-buffer term-name))
      (elastic--add-terminal term-name buf 'child))
    ;; Now show that buffer in a new or existing tab in the CF
    (with-selected-frame cf
      (tab-bar-new-tab)
      ;; rename the tab to the term-name so it’s easy to identify
      (tab-bar-rename-tab term-name)
      (switch-to-buffer buf)
      (make-frame-visible cf)
      (select-frame-set-input-focus cf))))

(defun elastic--show-main-frame-terminal (term-name)
  "Show or create a vterm named TERM-NAME inside a new tab on the MAIN frame."
  (let* ((term-pair (elastic--get-terminal term-name))
         (buf (car term-pair)))
    (unless (and buf (buffer-live-p buf))
      (setq buf (elastic--create-vterm-buffer term-name))
      (elastic--add-terminal term-name buf 'main))
    ;; Show buffer in the main frame, in a new tab
    (with-selected-frame elastic--floating-frame-parent
      (tab-bar-new-tab)
      (tab-bar-rename-tab term-name)
      (switch-to-buffer buf)
      (select-frame-set-input-focus elastic--floating-frame-parent))))

;;; Terminal Toggle

(defun elastic-toggle-term (term-name mode)
  "Toggle visibility of a terminal named TERM-NAME in the current persp, with MODE.
MODE is either 'child (floating) or 'main (tab in main frame).
If terminal doesn't exist, create it. If it exists and is visible, hide it, etc.

This is your universal toggling function. 
You can bind M-o, M-j, M-t to calls to this function with different arguments:
- (elastic-toggle-term \"main\" 'main) => M-o
- (elastic-toggle-term \"fmain\" 'child) => M-t
- (elastic-toggle-term \"build\" 'child) => M-j
"
  (interactive
   (list (read-string "Terminal name: ")
         (intern (completing-read "Mode (child/main): " '("child" "main") nil t))))
  (let* ((cf (elastic--get-child-frame))
         (term-pair (elastic--get-terminal term-name))
         (buf (car term-pair))
         (existing-mode (cdr term-pair)))
    (cond
     ;; If the terminal doesn't exist at all, create it in the requested mode.
     ((not term-pair)
      (if (eq mode 'child)
          (elastic--show-child-frame-terminal term-name)
        (elastic--show-main-frame-terminal term-name)))

     ;; If the terminal exists in the child frame.
     ((eq existing-mode 'child)
      (if (elastic--child-frame-visible-p cf)
          ;; Hide child frame if visible
          (make-frame-invisible cf)
        ;; Show it (and show the correct tab):
        (elastic--show-child-frame-terminal term-name)))

     ;; If the terminal exists in the main frame.
     ((eq existing-mode 'main)
      ;; Toggling “visibility” for main-frame tabs is trickier:
      ;; We can “hide it” by switching to another tab, or killing the tab, etc.
      ;; For a quick approach, we can see if it's in the current tab,
      ;; and switch away or close it. Or just create/show it again:
      (let ((mf elastic--floating-frame-parent))
        (with-selected-frame mf
          (if (get-buffer-window buf mf)
              ;; If it's visible in current tab, let's bury it or switch tab
              (progn
                (tab-bar-switch-to-next-tab)
                (bury-buffer buf))
            ;; Otherwise show it
            (elastic--show-main-frame-terminal term-name))))))))

;;; Example Keybindings for the 3 terminals (per persp)

;; M-o => main terminal in main frame
;; M-j => build in child frame
;; M-t => fmain in child frame
;;
;; You can place these in your config:
;; (define-key evil-normal-state-map (kbd "M-o") (lambda () (interactive)
;;   (elastic-toggle-term "main" 'main)))
;; (define-key evil-normal-state-map (kbd "M-j") (lambda () (interactive)
;;   (elastic-toggle-term "build" 'child)))
;; (define-key evil-normal-state-map (kbd "M-t") (lambda () (interactive)
;;   (elastic-toggle-term "fmain" 'child)))

;;; Hooking into perspective switching

(defun elastic--on-persp-switch ()
  "When switching to a new persp, hide old child's frame and show the new one if it exists."
  ;; This is optional logic, adjust to your liking.
  (let* ((all-persps (persp-names-current-frame-fast-lookup))
         (current-p (elastic--get-persp-name))
         (cf-new (elastic--get-child-frame current-p)))
    ;; Hide all child frames that belong to other persps
    (dolist (p all-persps)
      (unless (string= p current-p)
        (let ((cf-old (elastic--get-child-frame p)))
          (when (elastic--child-frame-visible-p cf-old)
            (make-frame-invisible cf-old)))))
    ;; Optionally, show the new child's frame if you want it auto-shown
    (when (and cf-new (frame-live-p cf-new))
      (make-frame-visible cf-new))))

(add-hook 'persp-before-switch-functions
          (lambda (&rest _args)
            ;; Possibly hide the frame from the perspective we are leaving
            (when-let ((old-persp (elastic--get-persp-name))
                       (cf-old (elastic--get-child-frame old-persp)))
              (when (elastic--child-frame-visible-p cf-old)
                (make-frame-invisible cf-old)))))

(add-hook 'persp-activated-hook #'elastic--on-persp-switch)

(provide 'elastic)
;;; elastic.el ends here
