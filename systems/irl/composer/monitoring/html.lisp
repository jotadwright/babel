(in-package :irl)

;; -------------------------------------
;; + make html for composition process +
;; -------------------------------------


(add-element `((h3) ((a ,@(make-expand/collapse-link-parameters
                            0 t "composition process"))
                      "Composition process")))

(defun make-collapsed-html-for-composition-process (composer element-id)
  (let ((solutions
         (when (solutions composer)
           (mapcar #'node (solutions composer)))))
    `((div)
      ((h3) ((a ,@(make-expand/collapse-link-parameters
                   element-id t "composition process"))
             "Composition process"))
      ((p :style "margin-left: 45px;") ((div :style "margin-top:-7px")
                                        ,(make-html (top composer) :expand-initially nil
                                                    :solutions solutions))))))

(defun make-expanded-html-for-composition-process (composer element-id)
  (let ((solutions
         (when (solutions composer)
           (mapcar #'node (solutions composer)))))
    `((div)
      ((h3) ((a ,@(make-expand/collapse-link-parameters
                   element-id nil "composition process"))
             "Composition process"))
      ((p :style "margin-left: 45px;") ((div :style "margin-top:-7px")
                                        ,(make-html (top composer) :expand-initially t
                                                    :solutions solutions))))))
  
(defmethod make-html-for-composition-process ((composer chunk-composer))
  (let ((element-id (make-id 'composition-process)))
    (make-expandable/collapsable-element
     element-id (make-id)
     (make-collapsed-html-for-composition-process composer element-id)
     (make-expanded-html-for-composition-process composer element-id))))
