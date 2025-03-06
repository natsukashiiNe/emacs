(make-frame`(
   (parent-frame . ,(selected-frame)) ;; Attach to the current frame
   (background-color . "#000000")
   (width . 60)    ;; 60 columns wide
   (height . 20)   ;; 20 rows tall
   (left . 100)    ;; Position relative to the parent
   (top . 50)      ;; Position relative to the parent
   (no-accept-focus . t) ;; Don't steal focus
   (visibility . t)
   (child-frame-border-width . 50)
   (border-color . "#FF0000")
   )
) 


(set-face-background 'child-frame-border "#FF0000")
