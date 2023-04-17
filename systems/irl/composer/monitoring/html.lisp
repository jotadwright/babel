(in-package :irl)

;; -------------------------------------
;; + make html for composition process +
;; -------------------------------------

(defun make-collapsed-html-for-composition-process (composer element-id)
  (let ((solutions
         (when (solutions composer)
           (mapcar #'node (solutions composer)))))
    `((table :class "two-col")
      ((tbody)
       ((tr)
        ((td)
         ((a ,@(make-expand/collapse-link-parameters
                element-id t "composition process"))
          "composition process"))
        ((td)
         ((div :style "margin-top:-7px")
          ,(make-html (top composer) :expand-initially nil
                      :solutions solutions))))))))

(defun make-expanded-html-for-composition-process (composer element-id)
  (let ((solutions
         (when (solutions composer)
           (mapcar #'node (solutions composer)))))
    `((table :class "two-col")
      ((tbody)
       ((tr)
        ((td)
         ((a ,@(make-expand/collapse-link-parameters
                element-id nil "composition process"))
          "composition process"))
        ((td)
         ((div :style "margin-top:-7px")
          ,(make-html (top composer) :expand-initially t
                      :solutions solutions))))))))
  
(defmethod make-html-for-composition-process ((composer chunk-composer))
  (let ((element-id (make-id 'composition-process)))
    (make-expandable/collapsable-element
     element-id (make-id)
     (make-collapsed-html-for-composition-process composer element-id)
     (make-expanded-html-for-composition-process composer element-id))))
